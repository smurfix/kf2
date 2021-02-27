void main(void) {
  switch (gl_VertexID) {
  default:
  case 0:
    gl_Position = vec4(-1.0, -1.0, 0.0, 1.0);
    break;
  case 1:
    gl_Position = vec4( 1.0, -1.0, 0.0, 1.0);
    break;
  case 2:
    gl_Position = vec4(-1.0,  1.0, 0.0, 1.0);
    break;
  case 3:
    gl_Position = vec4( 1.0,  1.0, 0.0, 1.0);
    break;
  }
}
