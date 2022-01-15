program FinanceLayerNewsApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  FinanceLayerNews.uMain in 'FinanceLayerNews.uMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
