#ifndef KF_PNG_H
#define KF_PNG_H 1

#include <string>

extern int SavePNG(const std::string &szFileName, char *Data, int nHeight, int nWidth, int nColors, const std::string &comment);
extern std::string ReadPNGComment(const std::string &filename);

#endif
