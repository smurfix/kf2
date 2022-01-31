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

	// mpfr_get_exp is documented as undefined for zero
	if(m_dec == 0)
		return "0";

    long expo = mpfr_get_exp(m_dec.backend().data());
	expo = expo*1000L/3322; // log10(2)
	int prec = m_dec.precision();

	if(-expo > 2*prec) {
		// don't switch to fixedpoint if that would add too many zeroes
		os << std::setprecision(prec + 3);
	} else if(expo < 0) {
		// increase precision by the number of leading zeroes
		os << std::fixed << std::setprecision(prec-expo + 3);
	} else {
		os << std::fixed << std::setprecision(prec + 3);
	}
	os << m_dec;

	std::string s = os.str();
	std::size_t e = s.find('e');
	if (e != std::string::npos) {
		s[e] = 'E';
	}
	else if (s.find('.') != std::string::npos)
	{
		// cut off trailing zeroes
		std::size_t n = s.length() - 1;
		while (s[n] == '0')
			s[n--] = 0;
		if(s[n] == '.')
			s[n] = 0;
	}
	
	return s;
}
