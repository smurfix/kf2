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
