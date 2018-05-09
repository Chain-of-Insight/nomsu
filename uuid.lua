-- A simple UUID function based on RFC 4122: http://www.ietf.org/rfc/rfc4122.txt
local function uuid()
    local r = math.random
    -- Set all the other bits to randomly (or pseudo-randomly) chosen values.
    local bytes = {
        r(2^(4*8)), --time-low
        r(2^(2*8)), --time-mid
        r(2^(2*8-4)), --time-high-and-version
        r(2^(1*8-2)), --clock-seq-and-reserved
        r(2^(1*8)), --clock-seq-low
        r(2^(3*8)), r(2^(3*8)), --node
    } 
    -- Set the four most significant bits (bits 12 through 15) of the
    -- time_hi_and_version field to the 4-bit version number from
    -- Section 4.1.3.
    bytes[3] = bytes[3] + 0x4000
    -- Set the two most significant bits (bits 6 and 7) of the
    -- clock_seq_hi_and_reserved to zero and one, respectively.
    bytes[4] = bytes[4] + 0xC0
    return ("%08x-%04x-%04x-%02x%02x-%6x%6x"):format(unpack(bytes))
end

return uuid
