#ifndef KF_JPEG_H
#define KF_JPEG_H 1

#include <string>

extern int SaveJPG(const std::string &szFileName, char *Data, int nHeight, int nWidth, int nColors, int nQuality, const std::string &comment);
extern std::string ReadJPEGComment(const std::string &filename);

#endif
