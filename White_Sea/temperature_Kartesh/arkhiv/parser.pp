{$mode delphi}
var
  t: text;
function extractdate: string;
var
 i, p: integer;
begin
  readln(t, Result);
  for i := 1 to 8 do
    begin
      p := pos(',', Result);
      delete(Result, 1, p);
    end;
  
end;

function ExtractValue(s: string): string;
var
 p: integer;
begin
  delete(s, 1, 1);
  p := pos(',', s);
  delete(s, 1, p);
  p := pos(',', s);
  delete(s, p, length(s));
  Result := s +',';
end;

var
  s, datestr: string;
  waiting: boolean;
  outf:text;
begin
  waiting := false;
  assign(t, paramstr(1));
  assign(outf, 'OUT_'+paramstr(1));
  rewrite(outf);
  reset(t);
  while not eof(t) do
    begin
      readln(t, s);
      if pos('LAT DEG', s) = 1 then
        datestr := extractdate+','
      else if (pos(',', s) = 1) and (length(s)>1) and (s[2] in['-','0'..'9']) then
        begin
          waiting := true;
          datestr := datestr + ExtractValue(s);
        end
      else if waiting then
        begin
          delete(datestr, length(datestr),1);
          writeln(outf, datestr);
          datestr := '';
          waiting := false;
        end;
    end;
  close(t);
  close(outf);
end.