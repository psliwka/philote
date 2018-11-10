#!/usr/bin/lua

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
	local cmd
	if module.params.config then
		cmd = "uci export " .. module.params.config
	else
		cmd = "uci export"
	end
	rc, stdout, stderr = module:run_command(cmd)
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
