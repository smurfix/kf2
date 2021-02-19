#version 330 core

out vec2 Internal_TexCoord;

void main(void) {
  switch (gl_VertexID) {
  default:
  case 0:
    Internal_TexCoord = vec2(0.0, 0.0);
    gl_Position = vec4(-1.0, -1.0, 0.0, 1.0);
    break;
  case 1:
    Internal_TexCoord = vec2(1.0, 0.0);
    gl_Position = vec4( 1.0, -1.0, 0.0, 1.0);
    break;
  case 2:
    Internal_TexCoord = vec2(0.0, 1.0);
    gl_Position = vec4(-1.0,  1.0, 0.0, 1.0);
    break;
  case 3:
    Internal_TexCoord = vec2(1.0, 1.0);
    gl_Position = vec4( 1.0,  1.0, 0.0, 1.0);
    break;
  }
}
