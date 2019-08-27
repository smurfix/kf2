% Kalles Fraktaler 2
% Copyright (C) 2013-2017 Karl Runmo
% Copyright (C) 2017-2019 Claude Heiland-Allen
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as
% published by the Free Software Foundation, either version 3 of the
% License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Affero General Public License for more details.
%
% You should have received a copy of the GNU Affero General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.

function stratify(stem, factor)

pattern = sprintf('%s-%s-%s.png', stem, '%04d', '%04d');
output = sprintf('%s.png', stem);

for y=1:factor
  for x=1:factor
    filename = sprintf(pattern, y - 1, x - 1);
    image = imread(filename);
    if (x == 1)
      row = image;
    else
      row = [ row , image ];
    end
  end
  if (y == 1)
    tiled = row;
  else
    tiled = [ tiled ; row ];
  end
end

[height, width, channels] = size(image);
tiled = reshape(tiled, [ height, factor, width, factor, channels ]);
tiled = permute(tiled, [ 2, 1, 4, 3, 5 ]);
tiled = reshape(tiled, [ factor * height, factor * width, channels ]);

imwrite(tiled, output);

endfunction
