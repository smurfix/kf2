import numpy as np
M=np.matrix

unit = M([[1,0],[0,1]])

def prm(x,m):
    print(f"{x}: [{m[0,0]} {m[0,1]} {m[0,2]} | {m[1,0]} {m[1,1]} {m[1,2]} | {m[2,0]} {m[2,1]} {m[2,2]}]");

def prv(x,m):
    print(f"{x}: [{m[0]} {m[1]} {m[2]}]");


# This should produce a matrix that mimics GetPixelCoordinates()
def mat(x,y,m):
    # x,y: input coordinates, 0…nX/0…nY
    # m: 2d matrix representing the polar stuff
    # f: scaling factor
    ms = M(m)
    ms = np.append(ms,[[0],[0]],axis=1)
    ms = np.append(ms,[[0,0,1]],axis=0)

    # transform x=(0,x) y=(0,y) to x=(-x/y,x/y), y=(-1,1)
    # so that (x/2,y/2) > (0,0)
    mi = M([
        [2/y,0  ,-1],
        [0  ,2/y,-1],
        [0  ,0  , 1],
        ])

    # mi: scale down to (-1,1)
    # ms: skew/rotate
    prm("Skew",ms);
    prm("Unit",mi);
    mi = ms @ mi
    prm("Res",mi);
    return mi

def ptest(a,b):
    # given two inputs (nx,ny, cx,cy,radius, rotation), calculate a map that
    # transposes one to the other
    a_nx,a_ny, a_x,a_y,a_z, a_m = a
    b_nx,b_ny, b_x,b_y,b_z, b_m = b
    z=a_z/b_z

    dx=(b_x-a_x)*a_z
    dy=(b_y-a_y)*a_z

    # the input is mapped to A
    print("dxy",dx,dy)
    print("A")
    print(a)
    m_a = mat(a_nx,a_ny, a_m)
    print("B",z)
    print(b)
    m_b = mat(b_nx,b_ny, b_m)

    print("One")
    print(m_a)
    print("One Rev")
    print(m_a.I)
    print("Two")
    print(m_b)

    # m_x transforms screen to math.
    # We want to go from screen B via math to screen A,
    # scaling and shifting the math part.
    m_shift = M([[1,0,dx],[0,1,dy],[0,0,1]])
    m_factor = M([[z,0,0],[0,z,0],[0,0,1]])
    prm("Sft",m_shift);
    prm("Fkt",m_factor);
    res = m_a.I@m_shift@m_factor@m_b
    prm("RES",res);
    return res;


def pt(x,a,b):
    m=ptest(a,b)
    print(x)
    print(a)
    print(b)
    print(m)
    print()
    return m

p1 = (100,100, 0,0,1, unit)
p2 = (200,200, 1,0,10, M([[1,0],[0.1,1]]))
#pt("Unit",p1,p1)
r=pt("shift right",p1,p2)
p=M([100,100,1]).T
print(p,"\n",r@p)
p=M([0,100,1]).T
print(p,"\n",r@p)

