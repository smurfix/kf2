#include "CDecNumber.h"

#include <iomanip>
#include <sstream>
#include <string>

std::string CDecNumber::ToText() const
{
	std::ostringstream os;
	os << std::fixed << std::setprecision(m_dec.precision() + 3) << m_dec;
	std::string s = os.str();
	std::size_t e = s.find('e');
	if (e != std::string::npos)
		s[e] = 'E';
	if (s.find('.') != std::string::npos && e == std::string::npos)
	{
		std::size_t n = s.length() - 1;
		while (s[n] == '0')
			s[n--] = 0;
		if(s[n] == '.')
			s[n] = 0;
	}
	else
	if (s[0] == '0' && s[1] == 'E')
	{
		s[1] = 0;
	}
	else if (e != std::string::npos)
	{
		int nExp = atoi(s.c_str() + e + 1);
		if (nExp > 0)
		{
			int i;
			for (i = 0; s[i] && s[i] != '.' && s[i+1] != 'E'; i++)
				;
			while (nExp){
				if (s[i+1] == 'E')
				{
					if (i == 0)
						i++;
					break;
				}
				s[i] = s[i+1];
				nExp--;
				i++;
			}
			s.replace(s.begin() + i, s.end(), nExp, '0');
		}
		e = s.find('E');
		while (e != std::string::npos && e != 0 && (s[e-1] == '0' || s[e-1] == '.'))
		{
			s.replace(s.begin() + e-1, s.end(), s.begin() + e, s.end());
			e--;
		}
	}
	return s;
}
