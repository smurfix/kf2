#ifndef KF_JPEG_H
#define KF_JPEG_H 1

#include <string>

extern int SaveJPG(char *szFileName, char *Data, int nHeight, int nWidth, int nColors, int nQuality, const char *comment);
extern std::string ReadJPEGComment(const std::string &filename);

#endif
