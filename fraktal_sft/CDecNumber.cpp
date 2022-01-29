/*
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2018 Claude Heiland-Allen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#include "CDecNumber.h"

#include <iomanip>
#include <sstream>
#include <string>

std::string str_rtrim(std::string x)
{
       x.erase(x.find_last_not_of("\t\n\v\f\r ") + 1);
       return x;
}

std::string CDecNumber::ToText() const
{
	std::ostringstream os;
	os << std::setprecision(m_dec.precision() + 3) << m_dec;
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
