#include <stdarg.h>

#include "StringHelper.h"

#include <vector>

int64_t str_atoi(std::string_view data)
{
	const std::string_view odata = data;

	int64_t val = 0;
	bool neg;

	if(data.length() == 0)
		return 0;
	neg = data.front() == '-';
	if(neg) {
		data.remove_prefix(1);
		if(data.length() == 0)
			return 0;
	}
	for (const auto& c : data) {
		if (c < '0' || '9' < c) {
			throw_invalid("not a number",odata);
		}
		val = val*10 + c-'0';
	}
	if(neg)
		val = -val;
	return val;
}
double str_atof(std::string_view data)
{
	const std::string_view odata = data;

	if(data.length() == 0)
		return 0;

	char *ep = const_cast<char*>(data.data())+data.length();
	char epc;
	if(*ep)
		epc = *ep;
	else
		ep = nullptr;

	char *ep2;
	double val = strtod(data.data(), &ep2);
	if(ep)
		*ep = epc;
	if(ep2 != data.data()+data.length())
		throw_invalid("not a number",odata);
	return val;
}

std::vector<std::string_view> str_split(const std::string_view i_str, const char *sep)
{
	std::vector<std::string_view> result;

	size_t found = i_str.find(sep);
	size_t startIndex = 0;
	size_t slen = strlen(sep);

	while(found != std::string::npos)
	{
		result.push_back(std::string_view(i_str.begin()+startIndex, found-startIndex));
		startIndex = found + slen;
		found = i_str.find(sep, startIndex);
	}
	if(startIndex != i_str.size())
		result.push_back(std::string_view(i_str.begin()+startIndex, i_str.size()-startIndex));
	return result;
}

std::vector<std::string> str_split_copy(const std::string_view i_str, const char *sep)
{
	std::vector<std::string> result;

	size_t found = i_str.find(sep);
	size_t startIndex = 0;
	size_t slen = strlen(sep);

	while(found != std::string::npos)
	{
		result.push_back(std::string(i_str.begin()+startIndex, found-startIndex));
		startIndex = found + slen;
		found = i_str.find(sep, startIndex);
	}
	if(startIndex != i_str.size())
		result.push_back(std::string(i_str.begin()+startIndex, i_str.size()-startIndex));
	return result;
}

std::string str_format(const std::string &fmt, ...) {
	int size=100;
	std::string str;
	va_list ap;

	str.resize(size);
	va_start(ap, fmt);
	int n = vsnprintf(str.data(), size, fmt.c_str(), ap);
	va_end(ap);

	if(n >= size) {
		str.resize(n+1);
		va_start(ap, fmt);
		n = vsnprintf(str.data(), size, fmt.c_str(), ap);
		va_end(ap);
	}
	if (n == -1)
		abort(); // XXX dunno what else to do
	else if (n < size)
		str.resize(n); // Make sure there are no trailing zero char
	return str;
}

KeyVal str_keyval(const std::string_view i_str, const char* sep) {
	std::size_t found = i_str.find(sep);
	if(found == std::string::npos || found == 0)
		return KeyVal("","");
	return KeyVal(std::string_view(i_str).substr(0,found),std::string_view(i_str).substr(found+std::strlen(sep)));
}
