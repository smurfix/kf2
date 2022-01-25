#include <glm/glm.hpp>
#include <stdio.h>
#include <iostream>

typedef glm::dvec3 vec3;
typedef glm::dmat3 mat3;
typedef glm::dmat2 mat2;

// This is a straight translation from the Python version.
// Careful about operator order: mat3's a*b is Python's b@a.
//
std::ostream &operator<<(std::ostream &s, const mat3 &x) {
    s<<"["
    <<x[0][0]<<" "
    <<x[0][1]<<" "
    <<x[0][2]<<" | "
    <<x[1][0]<<" "
    <<x[1][1]<<" "
    <<x[1][2]<<" | "
    <<x[2][0]<<" "
    <<x[2][1]<<" "
    <<x[2][2]<<"]";
    return s;
}

std::ostream &operator<<(std::ostream &s, const vec3 &x) {
    s<<"["
    <<x[0]<<" "
    <<x[1]<<" "
    <<x[2]<<"]";
    return s;
}

mat3 to_unit(int x,int y, const mat2 &skew)
{
    const mat3 mskew{
        skew[0][0], skew[0][1], 0,
        skew[1][0], skew[1][1], 0,
        0,0,1
    };
    std::cout<<"Skew: "<<mskew<<std::endl;

    const mat3 to_unit{
        2/(double)y, 0, -1,
        0, 2/(double)y, -1,
        0,0,1
    };
    std::cout<<"Unit: "<<to_unit<<std::endl;
    const mat3 res = to_unit * mskew;
    std::cerr << "Res: "<< res << std::endl;
    return res;
}

struct cfg {
    int nx,ny;
    double cx,cy,cz;
    mat2 skew;
};

mat3 transform(const cfg &a, const cfg &b)
{
    mat3 ma{to_unit(a.nx,a.ny, a.skew)};
    mat3 mb{to_unit(b.nx,b.ny, b.skew)};
    double dx = (b.cx-a.cx)*a.cz;
    double dy = (b.cy-a.cy)*a.cz;
    double dz = a.cz/b.cz;

    mat3 shift{1,0,dx, 0,1,dy, 0,0,1};
    mat3 factor{dz,0,0, 0,dz,0, 0,0,1};
    std::cout << "Sft:" << shift << std::endl;
    std::cout << "Fkt:" << factor << std::endl;
    mat3 res = mb*factor*shift*glm::inverse(ma);
    std::cout << "RES:" << res << std::endl;
    return res;
}

const cfg ca{100,100,0,0,1,mat2{1,0,0,1}};
const cfg cb{200,200,1,0,10,mat2{1,0,0.1,1}};

int main() {
    mat3 m = transform(ca,cb);

    {
    vec3 p1{100,100,1};
    vec3 pr = p1*m;
    printf("%f %f: %f %f\n", p1[0],p1[1], pr[0],pr[1]);
    }

    {
    vec3 p1{0,100,1};
    vec3 pr = p1*m;
    printf("%f %f: %f %f\n", p1[0],p1[1], pr[0],pr[1]);
    }
}
