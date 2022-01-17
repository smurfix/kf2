#ifndef COMMON_STRINGHELPER_H
#define COMMON_STRINGHELPER_H

#include <vector>
#include <string>
#include <cstring>
#include <boost/tokenizer.hpp>


// printf
std::string str_format(const std::string &fmt, ...);

static std::string strv_cat(const char *a, const char *b, std::string_view c)
{
    std::string res{a};
    res += b;
    res += c;
    return res;
}

static std::string strv_cat(const char *a, const char *b, const char* const c)
{
	return strv_cat(a,b,std::string_view(c));
}

static std::string strv_cat(const char *a, const char *b, const std::string& c)
{
	return strv_cat(a,b,std::string_view(c));
}

static std::string strv_cat(const char *a, const char *b, double c)
{
	return strv_cat(a,b,std::to_string(c));
}

static std::string strv_cat(const char *a, const char *b, int64_t c)
{
	return strv_cat(a,b,std::to_string(c));
}

static std::string strv_cat(const char *a, const char *b, int c)
{
	return strv_cat(a,b,std::to_string(c));
}

static std::string strv_cat(const char *a, const char *b, bool c)
{
	return strv_cat(a,b, c ? "true" : "false");
}

// error
template<typename T>
static inline void throw_invalid(const char *msg, T var) {
	throw std::invalid_argument(strv_cat(msg,": ",var));
}



int64_t str_atoi(std::string_view data);
double str_atof(std::string_view data);

// if you want a vector
std::vector<std::string> str_split(const std::string_view i_str, const char *sep);

// if you simply want to iterate
// for(const auto& t : str_iter("a b c"," ")) {…}
static inline boost::tokenizer<boost::char_separator<char>> str_iter(const std::string_view i_str, const char *sep)
{
	boost::char_separator<char> delim(sep);
	boost::tokenizer<boost::char_separator<char>> iter(i_str, delim);
	return iter;
}

// TODO create a simple key/value splitter
typedef std::pair<std::string_view,std::string_view> KeyVal;

static KeyVal str_keyval(const std::string_view i_str, const char* sep) {
	std::size_t found = i_str.find(sep);
	if(found == std::string::npos || found == 0)
		return KeyVal("","");
	return KeyVal(std::string_view(i_str).substr(0,found),std::string_view(i_str).substr(found+std::strlen(sep)));

}

#endif
