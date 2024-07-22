#!/usr/bin/env lua

local file_names = {
    "measurements-1.txt",
    "measurements-2.txt",
    "measurements-3.txt",
    "measurements-boundaries.txt",
    "measurements-complex-utf8.txt",
    "measurements-dot.txt",
    "measurements-short.txt",
    "measurements-shortest.txt",
    "measurements-10.txt",
    "measurements-20.txt",
    "measurements-10000-unique-keys.txt",
    "measurements-100000.txt",
    "measurements-1000000.txt",
    "measurements-10000000.txt",
    "measurements-1000000000.txt"
}

local checksums = {
    [1] = "b9ed83dbb95c47286d03510dc30b9f4e1c6f046d",  -- measurements-1.txt
    [2] = "74eaffb3336b503575d768bc5a08a818029eb833",  -- measurements-2.txt
    [3] = "7b245e058f13f3f1a3e281fce2e351df694bbaec",  -- measurements-3.txt
    [4] = "9566eec019eb757ac5a711dcc1c2488eed27c296",  -- measurements-boundaries.txt
    [5] = "763658233b962178d350adfc6e194f4c22e25faa",  -- measurements-complex-utf8.txt
    [6] = "e25655bd5c7c12b11204f71f4c5c734dd0261c34",  -- measurements-dot.txt
    [7] = "209f51a7821315bb22f14f31ce3f41432a115c84",  -- measurements-short.txt
    [8] = "3a90bce85485a77b2fb9758e846dc64d07455d1e",  -- measurements-shortest.txt
    [9] = "7e5dee8c7c7bf3fb130732e6ea1181366d727b5f",  -- measurements-10.txt
    [10] = "c2d5fc20e3240479471af7a61048d332bd943d96", -- measurements-20.txt
    [11] = "bf149bba3f79419901a15099de32c224c6e91022", -- measurements-1000-unique-keys.txt
    [12] = "df2d9b0fbf6f3d22e3be1ac0cbce29759bc9eed3", -- measurements-100000.txt
    [13] = "caec6a5e7d863946dde2c710ad69a71f37d1d951", -- measurements-1000000.txt
    [14] = "27f4b0a5007d58f47e8cef0e242449578ba1987e", -- measurements-10000000.txt
    [15] = "da6b764b0a8121ea5276bd3557cb7cf604bf301d"  -- measurements-1000000000.txt
}

local dir = arg[1]
if not dir then
    print("Usage: " .. arg[0] .. " <directory>")
    os.exit(1)
end

-- Ensure the provided argument is a directory
local handle = io.popen("test -d '" .. dir .. "' && echo 'true' || echo 'false'")
if not handle then
    print("Error: Failed to open directory check handle.")
    os.exit(1)
end

local result = handle:read("*a")
if not result then
    print("Error: Failed to read directory check result.")
    handle:close()
    os.exit(1)
end
handle:close()

result = result:gsub("%s+", "")
if result ~= "true" then
    print("Error: " .. dir .. " is not a directory.")
    os.exit(1)
end

for idx, file_name in ipairs(file_names) do
    local file = dir .. "/" .. file_name
    local key = file_name:match("measurements%-(.+)%..+")

    print("Processing file: " .. file)

    -- Check if the file exists
    local file_handle = io.open(file, "r")
    if file_handle then
        file_handle:close()

        -- Execute the command
        os.execute("1brc -f '" .. file .. "' > a.out")

        -- Compute the SHA-1 hash of the output file
        local hash_handle = io.popen("shasum a.out")
        if not hash_handle then
            print("Error: Failed to open hash computation handle.")
            os.exit(1)
        end

        local hash_result = hash_handle:read("*a")
        hash_handle:close()

        if not hash_result then
            print("Error: Failed to read hash computation result.")
            os.exit(1)
        end

        local calculated_hash = hash_result:match("^(%S+)")
        local expected_hash = checksums[idx]

        if calculated_hash ~= expected_hash then
            print("💥")
            print("Expected: " .. expected_hash)
            print("Received: " .. calculated_hash)
            os.exit(1)
        end
    else
        print("File " .. file .. " does not exist.")
    end
end
