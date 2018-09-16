#!/usr/bin/lua
	
do
	local _ENV = _ENV
	package.preload["ansible"] = function( ... )
		local arg = _G.arg;
		_ENV = _ENV;

		local Ansible = {}

local io   = require("io")
local json = require("dkjson")
local ubus = require("ubus")

Ansible.__index = Ansible

local json_arguments = [===[<<INCLUDE_ANSIBLE_MODULE_JSON_ARGS>>]===]

function Ansible.new(spec) 
	local self = setmetatable({}, Ansible)
	self.spec = spec
	for k,v in pairs(spec) do
		v['name'] = k
	end
	self.params = nil
	return self
end

local function split(str, delimiter)
	local toks = {}

	for tok in string.gmatch(str, "[^".. delimiter .. "]+") do
		toks[#toks + 1] = tok
	end

	return toks
end

local function append(t1, t2)
	for k,v in ipairs(t2) do
		t1[#t1 + 1] = v
	end
	return t1
end

function Ansible.contains(needle, haystack)
	for _,v in pairs(haystack) do
		if needle == v then
			return true
		end
	end

	return false
end

local function findspec(name, spec)
	if spec[name] then
		return spec[name]
	end

	-- check whether an alias exists
	for k,v in pairs(spec) do
		if type(v) == "table" and v['aliases'] then
			if Ansible.contains(name, v['aliases']) then
				return v
			end
		end
	end

	return nil
end

local function starts_with(str, start)
	return str:sub(1, #start) == start
end

local function extract_internal_ansible_params(params)
	local copy = {}
	for k,v in pairs(params) do
		if starts_with(k, "_ansible") then
			copy[k] = v
		end
	end
	return copy
end

local function canonicalize(params, spec)
	local copy = {}
	for k,v in pairs(params) do
		local desc = findspec(k, spec)
		if not desc then
			-- ignore _ansible parameters
			if 1 ~= string.find(k, "_ansible") then
				return nil, "no such parameter " .. k
			end
		else
			if copy[desc['name']] then
				return nil, "duplicate parameter " .. desc['name']
			end
			copy[desc['name']] = v
		end
	end

	params = copy

	return copy
end

function Ansible:slurp(path)
	local f, err = io.open(path, "r")
	if f == nil then
		Ansible.fail_json({msg="failed to open file " .. path .. ": " .. err})
	end
	local content = f:read("*a")
	if content == nil then
		self:fail_json({msg="read from file " .. path .. "failed"})
	end
	f:close()
	return content
end

function Ansible:unslurp(path, content)
	local f, err = io.open(path, "w+")
	if f == nil then
		Ansible.fail_json({msg="failed to open file " .. path .. ": " .. err})
	end
	
	local res = f:write(content)

	if not res then
		self:fail_json({msg="read from file " .. path .. "failed"})
	end
	f:close()
	return res
end

local function parse_dict_from_string(str)
	if 1 == string.find(str, "{") then
		-- assume json, try to decode it
		local dict, pos, err = json.decode(str)
		if not err then
			return dict
		end
	elseif string.find(str, "=") then
		fields = {}
		field_buffer = ""
		in_quote = nil
		in_escape = false
		for c in str:gmatch(".") do
			if in_escape then
				field_buffer = field_buffer .. c
				in_escape = false
			elseif c == '\\' then
				in_escape = true
			elseif not in_quote and ('\'' == c or '"' == c) then
				in_quote = c
			elseif in_quote and in_quote == c then
				in_quote = nil
			elseif not in_quote and (',' == c or ' ' == c) then
				if string.len(field_buffer) > 0 then
					fields[#fields + 1] = field_buffer
				end
				field_buffer=""
			else
				field_buffer = field_buffer .. c
			end
		end
		-- append the final field
		fields[#fields + 1] = field_buffer

		local dict = {}

		for _,v in ipairs(fields) do
			local key, val = string.match(v, "^([^=]+)=(.*)")

			if key and val then
				dict[key] = val
			end
		end

		return dict
	end

	return nil, str ..  " dictionary requested, could not parse JSON or key=value"
end

local function check_transform_type(variable, ansibletype)
	-- Types: str list dict bool int float path raw jsonarg
	if     "str"     == ansibletype then
		if type(variable) == "string" then
			return variable
		end
	elseif "list"    == ansibletype then
		if type(variable) == "table" then
			return variable
		end

		if type(variable) == "string" then
			return split(variable, ",")
		elseif type(variable) == "number" then
			return {variable}
		end
	elseif "dict"    == ansibletype then
		if type(variable) == "table" then
			return variable
		elseif type(variable) == "string" then
			return parse_dict_from_string(variable)
		end
	elseif "bool"    == ansibletype then
		if "boolean" == type(variable) then
			return variable
		elseif "number" == type(variable) then
			return not (0 == variable)
		elseif "string" == type(variable) then
			local BOOLEANS_TRUE  = {'yes', 'on', '1', 'true', 'True'}
			local BOOLEANS_FALSE = {'no', 'off', '0', 'false', 'False'}

			if Ansible.contains(variable, BOOLEANS_TRUE) then
				return true
			elseif Ansible.contains(variable, BOOLEANS_FALSE) then
				return false
			end
		end
	elseif "int"     == ansibletype or "float"   == ansibletype then
		if type(variable) == "string" then
			local var = tonumber(variable)
			if var then
				return var
			end
		elseif type(variable) == "number" then
			return variable
		end
	elseif "path"    == ansibletype then
		-- A bit basic, i know
		if type(variable) == "string" then
			return variable
		end
	elseif "raw"     == ansibletype then
		return variable
	elseif "jsonarg" == ansibletype then
		if     "table" == type(variable) then
			return variable
		elseif "string" == type(variable) then
			local dict, pos, err = json.decode(variable)
			if not err then
				return dict
			end
		end
	else
		return nil, ansibletype .. " is not a known type"
	end

	return nil, tostring(variable) .. " does not conform to type " .. ansibletype
end

function Ansible:parse(inputfile)
	local params, pos, err = json.decode(json_arguments)

	if err then
		self:fail_json({msg="INTERNAL: Illegal json input received"})
	end

	self.internal_params = extract_internal_ansible_params(params)
	self._diff = self.internal_params['_ansible_diff']

	-- resolve aliases
	params, err = canonicalize(params, self.spec)

	if not params then
		self:fail_json({msg="Err: " .. tostring(err)})
	end

	for k,v in pairs(self.spec) do
		-- setup defaults
		if v['default'] then
			if nil == params[k] then
				params[k] = v['default']
			end
		end

		-- assert requires
		if v['required'] then
			if not params[k] then
				self:fail_json({msg="Required parameter " .. k .. " not provided"})
			end
		end
	end
	
	-- check types/choices
	for k,v in pairs(params) do
		local typedesc = self.spec[k]['type']
		if typedesc then
			local val, err = check_transform_type(v, typedesc)
			if nil ~= val then
				params[k] = val
			else
				self:fail_json({msg="Err: " .. tostring(err)})
			end
		end

		local choices = self.spec[k]['choices']
		if choices then
			if not Ansible.contains(v, choices) then
				self:fail_json({msg=v .. " not a valid choice for " .. k})
			end
		end
	end

	self.params = params

	return params
end

local function file_exists(path)
	local f=io.open(path,"r")
	if f~=nil then
		io.close(f)
		return true
	else
		return false
	end
end

function Ansible:get_bin_path(name, required, candidates)
	if not candidates then
		candidates = {}
	end

	local path = os.getenv("PATH")
	if path then
		candidates = append(candidates, split(path, ":"))
	end

	for _,dir in pairs(candidates) do
		local fpath = dir .. "/" .. name
		if file_exists(fpath) then
			return fpath
		end
	end

	if required then
		self:fail_json({msg="No executable " .. name .. " found in PATH or candidates"})
	end
	
	return nil
end

function Ansible:remove_file(path)
	local rc, err = os.remove(path)
	if nil == rc then
		self:fail_json({msg="Internal, execute: failed to remove file " .. path})
	end
	return rc
end

local function get_version()
	local version = assert(string.match(_VERSION, "Lua (%d+.%d+)"))
	return tonumber(version) -- Aaaah, it hurts to use floating point like this...
end

function Ansible:run_command(command)
	local stdout = os.tmpname()
	local stderr = os.tmpname()

	local cmd = string.format("%s >%q 2>%q", command, stdout, stderr)

	local rc = nil
	if 5.1 < get_version() then
		_, _, rc = os.execute(cmd)
	else
		rc       = os.execute(cmd)
	end

	local out = self:slurp(stdout)
	local err = self:slurp(stderr)
	
	self:remove_file(stdout)
	self:remove_file(stderr)

	return rc, out, err
end

function Ansible:copy(src, dest)
	local command = string.format("cp -f %q %q", src, dest)
	local rc, _,  err = self:run_command(command)

	if rc ~= 0 then
		return false, err
	else
		return true, err
	end
end

function Ansible:move(src, dest)
	local command = string.format("mv -f %q %q", src, dest)
	local rc, _,  err = self:run_command(command)

	if rc ~= 0 then
		return false, err
	else
		return true, err
	end
end

function Ansible:fail_json(kwargs)
	assert(kwargs['msg'])
	kwargs['failed'] = true
	if nil == kwargs['changed'] then
		kwargs['changed'] = false
	end
	if nil == kwargs['invocation'] then
		kwargs['invocations'] = {module_args=self.params}
	end

	io.write(json.encode(kwargs))
	os.exit(1)
end

function Ansible:exit_json(kwargs)
	if nil == kwargs['changed'] then
		kwargs['changed'] = false
	end
	if nil == kwargs['invocation'] then
		kwargs['invocations'] = {module_args=self:get_params()}
	end

	io.write(json.encode(kwargs))
	os.exit(0)
end

function Ansible:get_params()
	return self.params
end

function Ansible:ubus_connect()
	local p = self:get_params()
	local timeout = p['timeout']
	if not timeout then
		timeout = 30
	end
	local socket = p['socket']

	local conn = ubus.connect(socket, timeout)
	if not conn then
		self:fail_json({msg="Failed to connect to ubus"})
	end

	return conn
end

function Ansible:ubus_call(conn, namespace, procedure, arg)
	local res, status = conn:call(namespace, procedure, arg)

	if nil ~= status and 0 ~= status then
		self:fail_json({msg="Ubus call failed", call={namespace=namespace, procedure=procedure, arg=arg, status=status}})
	end

	return res
end

function Ansible:backup_local(file)
	local backupdest

	if file_exits(file) then
		local ext = os.time("%Y-%m-%d@H:%M:%S~")

		backupdest = string.format("%s.%s", file, ext)

		local content = self:slurp(file)
		local res = self:unslurp(backupdest, content)
	end

	return backupdest
end

function Ansible:is_dir(path)
	local f, err, code = io.open(path, "r")

	if nil == f then
		return false, err, code
	end

	local ok, err, code = f:read(1)
	f:close()
	return code == 21, nil, nil
end

function Ansible:check_mode()
	return self.internal_params["_ansible_check_mode"]
end

return Ansible

	end
	package.preload["dkjson"] = function( ... )
		local arg = _G.arg;
		_ENV = _ENV;

		-- Module options:
local always_try_using_lpeg = true
local register_global_module_table = false
local global_module_name = 'json'

--[==[

David Kolf's JSON module for Lua 5.1/5.2

Version 2.5


For the documentation see the corresponding readme.txt or visit
<http://dkolf.de/src/dkjson-lua.fsl/>.

You can contact the author by sending an e-mail to 'david' at the
domain 'dkolf.de'.


Copyright (C) 2010-2013 David Heiko Kolf

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

--]==]

-- global dependencies:
local pairs, type, tostring, tonumber, getmetatable, setmetatable, rawset =
      pairs, type, tostring, tonumber, getmetatable, setmetatable, rawset
local error, require, pcall, select = error, require, pcall, select
local floor, huge = math.floor, math.huge
local strrep, gsub, strsub, strbyte, strchar, strfind, strlen, strformat =
      string.rep, string.gsub, string.sub, string.byte, string.char,
      string.find, string.len, string.format
local strmatch = string.match
local concat = table.concat

local json = { version = "dkjson 2.5" }

if register_global_module_table then
  _G[global_module_name] = json
end

local _ENV = nil -- blocking globals in Lua 5.2

pcall (function()
  -- Enable access to blocked metatables.
  -- Don't worry, this module doesn't change anything in them.
  local debmeta = require "debug".getmetatable
  if debmeta then getmetatable = debmeta end
end)

json.null = setmetatable ({}, {
  __tojson = function () return "null" end
})

local function isarray (tbl)
  local max, n, arraylen = 0, 0, 0
  for k,v in pairs (tbl) do
    if k == 'n' and type(v) == 'number' then
      arraylen = v
      if v > max then
        max = v
      end
    else
      if type(k) ~= 'number' or k < 1 or floor(k) ~= k then
        return false
      end
      if k > max then
        max = k
      end
      n = n + 1
    end
  end
  if max > 10 and max > arraylen and max > n * 2 then
    return false -- don't create an array with too many holes
  end
  return true, max
end

local escapecodes = {
  ["\""] = "\\\"", ["\\"] = "\\\\", ["\b"] = "\\b", ["\f"] = "\\f",
  ["\n"] = "\\n",  ["\r"] = "\\r",  ["\t"] = "\\t"
}

local function escapeutf8 (uchar)
  local value = escapecodes[uchar]
  if value then
    return value
  end
  local a, b, c, d = strbyte (uchar, 1, 4)
  a, b, c, d = a or 0, b or 0, c or 0, d or 0
  if a <= 0x7f then
    value = a
  elseif 0xc0 <= a and a <= 0xdf and b >= 0x80 then
    value = (a - 0xc0) * 0x40 + b - 0x80
  elseif 0xe0 <= a and a <= 0xef and b >= 0x80 and c >= 0x80 then
    value = ((a - 0xe0) * 0x40 + b - 0x80) * 0x40 + c - 0x80
  elseif 0xf0 <= a and a <= 0xf7 and b >= 0x80 and c >= 0x80 and d >= 0x80 then
    value = (((a - 0xf0) * 0x40 + b - 0x80) * 0x40 + c - 0x80) * 0x40 + d - 0x80
  else
    return ""
  end
  if value <= 0xffff then
    return strformat ("\\u%.4x", value)
  elseif value <= 0x10ffff then
    -- encode as UTF-16 surrogate pair
    value = value - 0x10000
    local highsur, lowsur = 0xD800 + floor (value/0x400), 0xDC00 + (value % 0x400)
    return strformat ("\\u%.4x\\u%.4x", highsur, lowsur)
  else
    return ""
  end
end

local function fsub (str, pattern, repl)
  -- gsub always builds a new string in a buffer, even when no match
  -- exists. First using find should be more efficient when most strings
  -- don't contain the pattern.
  if strfind (str, pattern) then
    return gsub (str, pattern, repl)
  else
    return str
  end
end

local function quotestring (value)
  -- based on the regexp "escapable" in https://github.com/douglascrockford/JSON-js
  value = fsub (value, "[%z\1-\31\"\\\127]", escapeutf8)
  if strfind (value, "[\194\216\220\225\226\239]") then
    value = fsub (value, "\194[\128-\159\173]", escapeutf8)
    value = fsub (value, "\216[\128-\132]", escapeutf8)
    value = fsub (value, "\220\143", escapeutf8)
    value = fsub (value, "\225\158[\180\181]", escapeutf8)
    value = fsub (value, "\226\128[\140-\143\168-\175]", escapeutf8)
    value = fsub (value, "\226\129[\160-\175]", escapeutf8)
    value = fsub (value, "\239\187\191", escapeutf8)
    value = fsub (value, "\239\191[\176-\191]", escapeutf8)
  end
  return "\"" .. value .. "\""
end
json.quotestring = quotestring

local function replace(str, o, n)
  local i, j = strfind (str, o, 1, true)
  if i then
    return strsub(str, 1, i-1) .. n .. strsub(str, j+1, -1)
  else
    return str
  end
end

-- locale independent num2str and str2num functions
local decpoint, numfilter

local function updatedecpoint ()
  decpoint = strmatch(tostring(0.5), "([^05+])")
  -- build a filter that can be used to remove group separators
  numfilter = "[^0-9%-%+eE" .. gsub(decpoint, "[%^%$%(%)%%%.%[%]%*%+%-%?]", "%%%0") .. "]+"
end

updatedecpoint()

local function num2str (num)
  return replace(fsub(tostring(num), numfilter, ""), decpoint, ".")
end

local function str2num (str)
  local num = tonumber(replace(str, ".", decpoint))
  if not num then
    updatedecpoint()
    num = tonumber(replace(str, ".", decpoint))
  end
  return num
end

local function addnewline2 (level, buffer, buflen)
  buffer[buflen+1] = "\n"
  buffer[buflen+2] = strrep ("  ", level)
  buflen = buflen + 2
  return buflen
end

function json.addnewline (state)
  if state.indent then
    state.bufferlen = addnewline2 (state.level or 0,
                           state.buffer, state.bufferlen or #(state.buffer))
  end
end

local encode2 -- forward declaration

local function addpair (key, value, prev, indent, level, buffer, buflen, tables, globalorder, state)
  local kt = type (key)
  if kt ~= 'string' and kt ~= 'number' then
    return nil, "type '" .. kt .. "' is not supported as a key by JSON."
  end
  if prev then
    buflen = buflen + 1
    buffer[buflen] = ","
  end
  if indent then
    buflen = addnewline2 (level, buffer, buflen)
  end
  buffer[buflen+1] = quotestring (key)
  buffer[buflen+2] = ":"
  return encode2 (value, indent, level, buffer, buflen + 2, tables, globalorder, state)
end

local function appendcustom(res, buffer, state)
  local buflen = state.bufferlen
  if type (res) == 'string' then
    buflen = buflen + 1
    buffer[buflen] = res
  end
  return buflen
end

local function exception(reason, value, state, buffer, buflen, defaultmessage)
  defaultmessage = defaultmessage or reason
  local handler = state.exception
  if not handler then
    return nil, defaultmessage
  else
    state.bufferlen = buflen
    local ret, msg = handler (reason, value, state, defaultmessage)
    if not ret then return nil, msg or defaultmessage end
    return appendcustom(ret, buffer, state)
  end
end

function json.encodeexception(reason, value, state, defaultmessage)
  return quotestring("<" .. defaultmessage .. ">")
end

encode2 = function (value, indent, level, buffer, buflen, tables, globalorder, state)
  local valtype = type (value)
  local valmeta = getmetatable (value)
  valmeta = type (valmeta) == 'table' and valmeta -- only tables
  local valtojson = valmeta and valmeta.__tojson
  if valtojson then
    if tables[value] then
      return exception('reference cycle', value, state, buffer, buflen)
    end
    tables[value] = true
    state.bufferlen = buflen
    local ret, msg = valtojson (value, state)
    if not ret then return exception('custom encoder failed', value, state, buffer, buflen, msg) end
    tables[value] = nil
    buflen = appendcustom(ret, buffer, state)
  elseif value == nil then
    buflen = buflen + 1
    buffer[buflen] = "null"
  elseif valtype == 'number' then
    local s
    if value ~= value or value >= huge or -value >= huge then
      -- This is the behaviour of the original JSON implementation.
      s = "null"
    else
      s = num2str (value)
    end
    buflen = buflen + 1
    buffer[buflen] = s
  elseif valtype == 'boolean' then
    buflen = buflen + 1
    buffer[buflen] = value and "true" or "false"
  elseif valtype == 'string' then
    buflen = buflen + 1
    buffer[buflen] = quotestring (value)
  elseif valtype == 'table' then
    if tables[value] then
      return exception('reference cycle', value, state, buffer, buflen)
    end
    tables[value] = true
    level = level + 1
    local isa, n = isarray (value)
    if n == 0 and valmeta and valmeta.__jsontype == 'object' then
      isa = false
    end
    local msg
    if isa then -- JSON array
      buflen = buflen + 1
      buffer[buflen] = "["
      for i = 1, n do
        buflen, msg = encode2 (value[i], indent, level, buffer, buflen, tables, globalorder, state)
        if not buflen then return nil, msg end
        if i < n then
          buflen = buflen + 1
          buffer[buflen] = ","
        end
      end
      buflen = buflen + 1
      buffer[buflen] = "]"
    else -- JSON object
      local prev = false
      buflen = buflen + 1
      buffer[buflen] = "{"
      local order = valmeta and valmeta.__jsonorder or globalorder
      if order then
        local used = {}
        n = #order
        for i = 1, n do
          local k = order[i]
          local v = value[k]
          if v then
            used[k] = true
            buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder, state)
            prev = true -- add a seperator before the next element
          end
        end
        for k,v in pairs (value) do
          if not used[k] then
            buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder, state)
            if not buflen then return nil, msg end
            prev = true -- add a seperator before the next element
          end
        end
      else -- unordered
        for k,v in pairs (value) do
          buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder, state)
          if not buflen then return nil, msg end
          prev = true -- add a seperator before the next element
        end
      end
      if indent then
        buflen = addnewline2 (level - 1, buffer, buflen)
      end
      buflen = buflen + 1
      buffer[buflen] = "}"
    end
    tables[value] = nil
  else
    return exception ('unsupported type', value, state, buffer, buflen,
      "type '" .. valtype .. "' is not supported by JSON.")
  end
  return buflen
end

function json.encode (value, state)
  state = state or {}
  local oldbuffer = state.buffer
  local buffer = oldbuffer or {}
  state.buffer = buffer
  updatedecpoint()
  local ret, msg = encode2 (value, state.indent, state.level or 0,
                   buffer, state.bufferlen or 0, state.tables or {}, state.keyorder, state)
  if not ret then
    error (msg, 2)
  elseif oldbuffer == buffer then
    state.bufferlen = ret
    return true
  else
    state.bufferlen = nil
    state.buffer = nil
    return concat (buffer)
  end
end

local function loc (str, where)
  local line, pos, linepos = 1, 1, 0
  while true do
    pos = strfind (str, "\n", pos, true)
    if pos and pos < where then
      line = line + 1
      linepos = pos
      pos = pos + 1
    else
      break
    end
  end
  return "line " .. line .. ", column " .. (where - linepos)
end

local function unterminated (str, what, where)
  return nil, strlen (str) + 1, "unterminated " .. what .. " at " .. loc (str, where)
end

local function scanwhite (str, pos)
  while true do
    pos = strfind (str, "%S", pos)
    if not pos then return nil end
    local sub2 = strsub (str, pos, pos + 1)
    if sub2 == "\239\187" and strsub (str, pos + 2, pos + 2) == "\191" then
      -- UTF-8 Byte Order Mark
      pos = pos + 3
    elseif sub2 == "//" then
      pos = strfind (str, "[\n\r]", pos + 2)
      if not pos then return nil end
    elseif sub2 == "/*" then
      pos = strfind (str, "*/", pos + 2)
      if not pos then return nil end
      pos = pos + 2
    else
      return pos
    end
  end
end

local escapechars = {
  ["\""] = "\"", ["\\"] = "\\", ["/"] = "/", ["b"] = "\b", ["f"] = "\f",
  ["n"] = "\n", ["r"] = "\r", ["t"] = "\t"
}

local function unichar (value)
  if value < 0 then
    return nil
  elseif value <= 0x007f then
    return strchar (value)
  elseif value <= 0x07ff then
    return strchar (0xc0 + floor(value/0x40),
                    0x80 + (floor(value) % 0x40))
  elseif value <= 0xffff then
    return strchar (0xe0 + floor(value/0x1000),
                    0x80 + (floor(value/0x40) % 0x40),
                    0x80 + (floor(value) % 0x40))
  elseif value <= 0x10ffff then
    return strchar (0xf0 + floor(value/0x40000),
                    0x80 + (floor(value/0x1000) % 0x40),
                    0x80 + (floor(value/0x40) % 0x40),
                    0x80 + (floor(value) % 0x40))
  else
    return nil
  end
end

local function scanstring (str, pos)
  local lastpos = pos + 1
  local buffer, n = {}, 0
  while true do
    local nextpos = strfind (str, "[\"\\]", lastpos)
    if not nextpos then
      return unterminated (str, "string", pos)
    end
    if nextpos > lastpos then
      n = n + 1
      buffer[n] = strsub (str, lastpos, nextpos - 1)
    end
    if strsub (str, nextpos, nextpos) == "\"" then
      lastpos = nextpos + 1
      break
    else
      local escchar = strsub (str, nextpos + 1, nextpos + 1)
      local value
      if escchar == "u" then
        value = tonumber (strsub (str, nextpos + 2, nextpos + 5), 16)
        if value then
          local value2
          if 0xD800 <= value and value <= 0xDBff then
            -- we have the high surrogate of UTF-16. Check if there is a
            -- low surrogate escaped nearby to combine them.
            if strsub (str, nextpos + 6, nextpos + 7) == "\\u" then
              value2 = tonumber (strsub (str, nextpos + 8, nextpos + 11), 16)
              if value2 and 0xDC00 <= value2 and value2 <= 0xDFFF then
                value = (value - 0xD800)  * 0x400 + (value2 - 0xDC00) + 0x10000
              else
                value2 = nil -- in case it was out of range for a low surrogate
              end
            end
          end
          value = value and unichar (value)
          if value then
            if value2 then
              lastpos = nextpos + 12
            else
              lastpos = nextpos + 6
            end
          end
        end
      end
      if not value then
        value = escapechars[escchar] or escchar
        lastpos = nextpos + 2
      end
      n = n + 1
      buffer[n] = value
    end
  end
  if n == 1 then
    return buffer[1], lastpos
  elseif n > 1 then
    return concat (buffer), lastpos
  else
    return "", lastpos
  end
end

local scanvalue -- forward declaration

local function scantable (what, closechar, str, startpos, nullval, objectmeta, arraymeta)
  local len = strlen (str)
  local tbl, n = {}, 0
  local pos = startpos + 1
  if what == 'object' then
    setmetatable (tbl, objectmeta)
  else
    setmetatable (tbl, arraymeta)
  end
  while true do
    pos = scanwhite (str, pos)
    if not pos then return unterminated (str, what, startpos) end
    local char = strsub (str, pos, pos)
    if char == closechar then
      return tbl, pos + 1
    end
    local val1, err
    val1, pos, err = scanvalue (str, pos, nullval, objectmeta, arraymeta)
    if err then return nil, pos, err end
    pos = scanwhite (str, pos)
    if not pos then return unterminated (str, what, startpos) end
    char = strsub (str, pos, pos)
    if char == ":" then
      if val1 == nil then
        return nil, pos, "cannot use nil as table index (at " .. loc (str, pos) .. ")"
      end
      pos = scanwhite (str, pos + 1)
      if not pos then return unterminated (str, what, startpos) end
      local val2
      val2, pos, err = scanvalue (str, pos, nullval, objectmeta, arraymeta)
      if err then return nil, pos, err end
      tbl[val1] = val2
      pos = scanwhite (str, pos)
      if not pos then return unterminated (str, what, startpos) end
      char = strsub (str, pos, pos)
    else
      n = n + 1
      tbl[n] = val1
    end
    if char == "," then
      pos = pos + 1
    end
  end
end

scanvalue = function (str, pos, nullval, objectmeta, arraymeta)
  pos = pos or 1
  pos = scanwhite (str, pos)
  if not pos then
    return nil, strlen (str) + 1, "no valid JSON value (reached the end)"
  end
  local char = strsub (str, pos, pos)
  if char == "{" then
    return scantable ('object', "}", str, pos, nullval, objectmeta, arraymeta)
  elseif char == "[" then
    return scantable ('array', "]", str, pos, nullval, objectmeta, arraymeta)
  elseif char == "\"" then
    return scanstring (str, pos)
  else
    local pstart, pend = strfind (str, "^%-?[%d%.]+[eE]?[%+%-]?%d*", pos)
    if pstart then
      local number = str2num (strsub (str, pstart, pend))
      if number then
        return number, pend + 1
      end
    end
    pstart, pend = strfind (str, "^%a%w*", pos)
    if pstart then
      local name = strsub (str, pstart, pend)
      if name == "true" then
        return true, pend + 1
      elseif name == "false" then
        return false, pend + 1
      elseif name == "null" then
        return nullval, pend + 1
      end
    end
    return nil, pos, "no valid JSON value at " .. loc (str, pos)
  end
end

local function optionalmetatables(...)
  if select("#", ...) > 0 then
    return ...
  else
    return {__jsontype = 'object'}, {__jsontype = 'array'}
  end
end

function json.decode (str, pos, nullval, ...)
  local objectmeta, arraymeta = optionalmetatables(...)
  return scanvalue (str, pos, nullval, objectmeta, arraymeta)
end

function json.use_lpeg ()
  local g = require ("lpeg")

  if g.version() == "0.11" then
    error "due to a bug in LPeg 0.11, it cannot be used for JSON matching"
  end

  local pegmatch = g.match
  local P, S, R = g.P, g.S, g.R

  local function ErrorCall (str, pos, msg, state)
    if not state.msg then
      state.msg = msg .. " at " .. loc (str, pos)
      state.pos = pos
    end
    return false
  end

  local function Err (msg)
    return g.Cmt (g.Cc (msg) * g.Carg (2), ErrorCall)
  end

  local SingleLineComment = P"//" * (1 - S"\n\r")^0
  local MultiLineComment = P"/*" * (1 - P"*/")^0 * P"*/"
  local Space = (S" \n\r\t" + P"\239\187\191" + SingleLineComment + MultiLineComment)^0

  local PlainChar = 1 - S"\"\\\n\r"
  local EscapeSequence = (P"\\" * g.C (S"\"\\/bfnrt" + Err "unsupported escape sequence")) / escapechars
  local HexDigit = R("09", "af", "AF")
  local function UTF16Surrogate (match, pos, high, low)
    high, low = tonumber (high, 16), tonumber (low, 16)
    if 0xD800 <= high and high <= 0xDBff and 0xDC00 <= low and low <= 0xDFFF then
      return true, unichar ((high - 0xD800)  * 0x400 + (low - 0xDC00) + 0x10000)
    else
      return false
    end
  end
  local function UTF16BMP (hex)
    return unichar (tonumber (hex, 16))
  end
  local U16Sequence = (P"\\u" * g.C (HexDigit * HexDigit * HexDigit * HexDigit))
  local UnicodeEscape = g.Cmt (U16Sequence * U16Sequence, UTF16Surrogate) + U16Sequence/UTF16BMP
  local Char = UnicodeEscape + EscapeSequence + PlainChar
  local String = P"\"" * g.Cs (Char ^ 0) * (P"\"" + Err "unterminated string")
  local Integer = P"-"^(-1) * (P"0" + (R"19" * R"09"^0))
  local Fractal = P"." * R"09"^0
  local Exponent = (S"eE") * (S"+-")^(-1) * R"09"^1
  local Number = (Integer * Fractal^(-1) * Exponent^(-1))/str2num
  local Constant = P"true" * g.Cc (true) + P"false" * g.Cc (false) + P"null" * g.Carg (1)
  local SimpleValue = Number + String + Constant
  local ArrayContent, ObjectContent

  -- The functions parsearray and parseobject parse only a single value/pair
  -- at a time and store them directly to avoid hitting the LPeg limits.
  local function parsearray (str, pos, nullval, state)
    local obj, cont
    local npos
    local t, nt = {}, 0
    repeat
      obj, cont, npos = pegmatch (ArrayContent, str, pos, nullval, state)
      if not npos then break end
      pos = npos
      nt = nt + 1
      t[nt] = obj
    until cont == 'last'
    return pos, setmetatable (t, state.arraymeta)
  end

  local function parseobject (str, pos, nullval, state)
    local obj, key, cont
    local npos
    local t = {}
    repeat
      key, obj, cont, npos = pegmatch (ObjectContent, str, pos, nullval, state)
      if not npos then break end
      pos = npos
      t[key] = obj
    until cont == 'last'
    return pos, setmetatable (t, state.objectmeta)
  end

  local Array = P"[" * g.Cmt (g.Carg(1) * g.Carg(2), parsearray) * Space * (P"]" + Err "']' expected")
  local Object = P"{" * g.Cmt (g.Carg(1) * g.Carg(2), parseobject) * Space * (P"}" + Err "'}' expected")
  local Value = Space * (Array + Object + SimpleValue)
  local ExpectedValue = Value + Space * Err "value expected"
  ArrayContent = Value * Space * (P"," * g.Cc'cont' + g.Cc'last') * g.Cp()
  local Pair = g.Cg (Space * String * Space * (P":" + Err "colon expected") * ExpectedValue)
  ObjectContent = Pair * Space * (P"," * g.Cc'cont' + g.Cc'last') * g.Cp()
  local DecodeValue = ExpectedValue * g.Cp ()

  function json.decode (str, pos, nullval, ...)
    local state = {}
    state.objectmeta, state.arraymeta = optionalmetatables(...)
    local obj, retpos = pegmatch (DecodeValue, str, pos, nullval, state)
    if state.msg then
      return nil, state.pos, state.msg
    else
      return obj, retpos
    end
  end

  -- use this function only once:
  json.use_lpeg = function () return json end

  json.using_lpeg = true

  return json -- so you can get the module using json = require "dkjson".use_lpeg()
end

if always_try_using_lpeg then
  pcall (json.use_lpeg)
end

return json


	end

end

local Ansible = require("ansible")

local module = Ansible.new({
	path            = { aliases = {"key", "name"}, type="str"},
	config          = { type="str" },
	section         = { type="str" },
	option          = { type="str" },
	type            = { type="str" },
	value           = { type="raw" },
	values          = { type="dict" },
	matching_values = { aliases = {"matching"}, type="dict" },
	state           = { default="present", choices={"present", "absent", "committed", "reverted"} },
	autocommit      = { default=true, type="bool" },
})

local conn = nil

function ubus_connect()
	conn = module:ubus_connect()
end

function assert_path_specified_once()
	if module.params.path then
		if module.params.config or module.params.section or module.params.option then
			module:fail_json{msg="'config', 'section' and 'option' parameters can not be used if 'path', 'key' or 'name' is specified."}
		end
	end
end

function assert_values_specified_once()
	if module.params.values then
		if module.params.option or module.params.value then
			module:fail_json{msg="Please specify either 'values' or 'option' and 'value' pair, not both."}
		end
	end
end

function validate_params()
	assert_path_specified_once()
	assert_values_specified_once()
end

local changed = false

local diff = {}

function populate_diff(moment)
	if not module._diff then return end
	rc, stdout, stderr = module:run_command("uci export")
	-- FIXME: check for errors
	diff[moment] = stdout
end

function canonicalize_path()
	-- a path consists of config.section.option
	local path = module.params.path
	if not path then
		-- path not specified; nothing to do
		return
	end

	-- lua's pattern engine does not seem to be expressive enough to do this in one go
	local config, section, option
	if string.match(path, "([^.]+)%.([^.]+)%.([^.]+)") then
		config, section, option = string.match(path, "([^.]+)%.([^.]+)%.([^.]+)")
	elseif string.match(path, "([^.]+)%.([^.]+)") then
		config, section = string.match(path, "([^.]+)%.([^.]+)")
	else
		config = path
	end

	module.params.config = config
	module.params.section = section
	module.params.option = option
end

function stringify_table(x)
	for k, v in pairs(x) do
		if type(v) == "table" then
			stringify_table(v)
		else
			x[k] = tostring(v)
		end
	end
end

function canonicalize_values()
	if module.params.option and module.params.value then
		module.params.values = { [module.params.option]=module.params.value }
	end
	if module.params.values then
		stringify_table(module.params.values)
	end
end

function canonicalize_params()
	canonicalize_path()
	canonicalize_values()
end

function get_changes()
	return module:ubus_call(conn, "uci", "changes", {config=module.params.config})["changes"]
end

function assert_autocommit_safe()
	local config = module.params.config
	if not module.params.autocommit then
		return
	elseif next(get_changes()) ~= nil then
		module:fail_json{msg="Uncommited changes detected in " .. config .. " config. It is not safe to perform autocommit. Please either disable autocommit, or ensure all " .. config .. " changes are committed (or reverted) prior to this task."}
	end
end

function autocommit_if_needed()
	if not module.params.autocommit then return end
	if not changed then return end
	module:ubus_call(conn, "uci", "commit", {config=module.params.config})
end

function section_matches(tested_values)
	if module.params.type and module.params.type ~= tested_values[".type"] then
		return false
	elseif module.params.matching_values and not section_values_match(tested_values, module.params.matching_values) then
		return false
	else
		return true
	end
end

function find_section()
	local sections = module:ubus_call(conn, "uci", "get", {config=module.params.config}).values
	local found_section = nil
	for name, values in pairs(sections) do
		if section_matches(values) then
			if not found_section then
				found_section = name
			else
				module:fail_json{msg="Multiple sections match specified conditions."}
			end
		end
	end
	module.params.section = found_section
end

function create_matching_section()
	res = module:ubus_call(conn, "uci", "add", {config=module.params.config, name=module.params.section, type=module.params.type, values=module.params.matching_values})
	module.params.section = res.section
end

function ensure_section_exists()
	if not module.params.section then find_section() end
	if not module.params.section or get_values() == nil then
		changed = true
		create_matching_section()
	end
end

function get_values()
	return module:ubus_call(conn, "uci", "get", {config=module.params.config, section=module.params.section}).values
end

function get_configs()
	return module:ubus_call(conn, "uci", "configs").configs
end

function arrays_equal(first, second)
	if #first ~= #second then return false end
	for index, value in pairs(first) do
		if second[index] ~= value then return false end
	end
	return true
end

function section_values_match(section_values, matching_pattern)
	for key, value_to_match in pairs(matching_pattern) do
		actual_value = section_values[key]
		if type(actual_value) ~= type(value_to_match) then
			return false
		elseif type(actual_value) == "table" then
			if not arrays_equal(actual_value, value_to_match) then
				return false
			end
		elseif actual_value ~= value_to_match then
			return false
		end
	end
	return true
end

function values_will_change()
	return not section_values_match(get_values(), module.params.values or {})
end

function set_values()
	if values_will_change() then
		changed = true
		module:ubus_call(conn, "uci", "set", {config=module.params.config, section=module.params.section, values=module.params.values})
	end
end

function ensure_present()
	assert_autocommit_safe()
	ensure_section_exists()
	set_values()
	autocommit_if_needed()
end

function delete()
	local current_values = get_values()
	if not current_values then return end
	if module.params.option and not current_values[module.params.option] then return end
	changed = true
	module:ubus_call(conn, "uci", "delete", {config=module.params.config, section=module.params.section, option=module.params.option})
end

function ensure_absent()
	assert_autocommit_safe()
	if not module.params.section then find_section() end
	delete()
	autocommit_if_needed()
end

function specified_or_all_configs()
	if module.params.config then
		return {module.params.config}
	else
		return get_configs()
	end
end

function config_has_uncommited_changes(config)
	if module:ubus_call(conn, "uci", "changes", {config=config}).changes then
		return true
	else
		return false
	end
end

function ensure_comitted()
	for _, config in pairs(specified_or_all_configs()) do
		if config_has_uncommited_changes(config) then
			changed = true
			module:ubus_call(conn, "uci", "commit", {config=config})
		end
	end
end

function ensure_reverted()
	for _, config in pairs(specified_or_all_configs()) do
		if config_has_uncommited_changes(config) then
			changed = true
			module:ubus_call(conn, "uci", "revert", {config=config})
		end
	end
end

function ensure_state()
	local state = module.params.state
	if state == "present" then
		ensure_present()
	elseif state == "absent" then
		ensure_absent()
	elseif state == "committed" then
		ensure_comitted()
	elseif state == "reverted" then
		ensure_reverted()
	end
end

function main()
	module:parse()
	validate_params()
	canonicalize_params()
	ubus_connect()
	populate_diff("before")
	ensure_state()
	populate_diff("after")
	module:exit_json({changed=changed, diff=diff})
end

main()
