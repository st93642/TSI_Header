#include <iostream>
#include <bitset>
#include <cstdint>

// Offline tutorial standard:
//   * Treat stdin as a four-bit mask (0-15) ordered as: front door, back door, side door, garage door.
//   * Declare constexpr unsigned int masks (FRONT_DOOR_MASK = 0b0001, etc.) so each bit meaning is explicit.
//   * Use bitwise AND to decide whether to print "Yes" or "No" for each sensor line.
// Expected output for input 5:
//   Mask: 5
//   Binary: 0101
//   Front Door Open: Yes
//   Back Door Open: No
//   Side Door Open: Yes
//   Garage Door Open: No
// Expected output for input 10:
//   Mask: 10
//   Binary: 1010
//   Front Door Open: No
//   Back Door Open: Yes
//   Side Door Open: No
//   Garage Door Open: Yes

int main() {
    // TODO: read an unsigned integer mask from std::cin
    // TODO: compute binary using std::bitset<4>
    // TODO: print the report exactly as shown above using "Yes" or "No"
    return 0;
}
