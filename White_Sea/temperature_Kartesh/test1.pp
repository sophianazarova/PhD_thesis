{$mode delphi}
uses
  sysutils, dateutils;
var
  x: TDateTime;
  DY: word;
  s, s1, s2: string;
  M_D_Y: array[1..3] of word;
  i, p: integer;
begin
  readln(s);//заголовки
  writeln('Day_Of_Year,', s);
  while not eof do
    begin
      readln(s);
      if s <> '' then
        begin
          s1 := s;
          for i := 1 to 3 do
            begin
              p := pos(',', s1);
              s2 := copy(s1, 1, p - 1);
              delete(s1, 1, p);
              val(s2, M_D_Y[i]);
            end;
          x := EncodeDate(M_D_Y[3]{Year}, M_D_Y[1]{Month}, M_D_Y[2]{Day});
          DY := DayOfTheYear(x);
          writeln(DY, ',', s);
        end;
    end;
end.