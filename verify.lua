#!/usr/bin/env lua

local file_names = {
    "measurements-boundaries.txt",
    "measurements-complex-utf8.txt",
    "measurements-dot.txt",
    "measurements-short.txt",
    "measurements-shortest.txt",
    "measurements-1.txt",
    "measurements-10.txt",
    "measurements-100.txt",
    "measurements-1000.txt",
    "measurements-10000.txt",
    "measurements-100000.txt",
    "measurements-1000000.txt",
    "measurements-10000000.txt",
    "measurements-100000000.txt",
    "measurements-1000000000.txt"
}

local checksums = {
    [1] = "9566eec019eb757ac5a711dcc1c2488eed27c296",  -- measurements-boundaries.out
    [2] = "763658233b962178d350adfc6e194f4c22e25faa",  -- measurements-complex-utf8.out
    [3] = "e25655bd5c7c12b11204f71f4c5c734dd0261c34",  -- measurements-dot.out
    [4] = "209f51a7821315bb22f14f31ce3f41432a115c84",  -- measurements-short.out
    [5] = "3a90bce85485a77b2fb9758e846dc64d07455d1e",  -- measurements-shortest.out
    [6] = "1cee441d648c59b95387c925854607fdc293ffea",  -- measurements-1.out
    [7] = "99cf6654daaeeab0aeb9c34935f408a1ba6d1734",  -- measurements-10.out
    [8] = "3f8ac812e47ee6b153ab5e2fee1e2fcefd3521b2",  -- measurements-100.out
    [9] = "af08d8e202ff8fa70605f93168016e5d2dd70550",  -- measurements-1000.out
    [10] = "b7e371280f13e46a0dd32c519c1d113ad7ad95da", -- measurements-10000.out
    [11] = "f682f944cc2113d87353d52f67399494c294c3dd", -- measurements-100000.out
    [12] = "6d5fd0a864a2165450d26cc8949ee3c5ac934550", -- measurements-1000000.out
    [13] = "c15afb6000715d76111c6c87f8fb9bd516f3b9d6", -- measurements-10000000.out
    [14] = "6d4cf50c39fafd4068d71899839593761be2f796", -- measurements-100000000.out
    [15] =
    "92b9efa4eeab2ff9ecfb68fb4efdb341e36678bb"         -- measurements-1000000000.out
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

    local file_handle = io.open(file, "r")
    if file_handle then
        file_handle:close()

        os.execute("1brc -f '" .. file .. "' > a.out")

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
