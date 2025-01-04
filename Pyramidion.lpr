program Pyramidion;

{$mode objfpc}{$H+}

uses
	  {$IFDEF UNIX}{$IFDEF UseCThreads}
	  cthreads,
	  {$ENDIF}{$ENDIF}
	  Interfaces, // this includes the LCL widgetset
	  Forms, Pyramidion019, uecontrols
	  { you can add units after this };

{$R *.res}

begin
     	  RequireDerivedFormResource:=True;
	  Application.Title:='Pyramidion 0.19';
          Application.Scaled:=True;
          Application.Initialize;
          Application.CreateForm(TAesthesiaForm, Form);
          Application.Run;
end.

