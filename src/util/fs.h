#ifndef FS_H
#define FS_H

// creates the namespace "fs" as a hack around <filesystem>
// compatibility issues.

#include <filesystem>

#if __cplusplus <= 199711L
  #error because it uses <filesystem>, we need c++11 or better
#endif

#ifdef __APPLE__
// sigh thanks apple
namespace fs = std::__fs::filesystem;
#else
namespace fs = std::filesystem;
#endif

#endif // FS_H

