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

#include "CFixedFloat.h"

#include <iomanip>
#include <sstream>
#include <string>

std::string CFixedFloat::ToText() const
{
  std::ostringstream os;
  os << std::fixed << std::setprecision(m_f.precision() + 3) << m_f;
	std::string s = os.str();
	std::size_t e = s.find('e');
	if (e != std::string::npos)
		s[e] = 'E';
	return s;
}
