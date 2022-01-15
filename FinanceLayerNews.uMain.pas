unit FinanceLayerNews.uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.DateTimeCtrls, FMX.Objects,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, System.Generics.Collections,
  System.JSON, REST.Types, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    Layout1: TLayout;
    VertScrollBox1: TVertScrollBox;
    DateEdit1: TDateEdit;
    Button2: TButton;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    procedure Button1Click(Sender: TObject);
  private
    RctNewsCard: TRectangle;
    LabelTitle: TLabel;
    LabelSource: TLabel;
    LabelURL: TLabel;
    MemoDescription: TMemo;

    RctList: TList<TRectangle>;
    LblTitleList: TList<TLabel>;
    LblSourceList: TList<TLabel>;
    LblURLList: TList<TLabel>;
    MemoList: TList<TMemo>;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  RESTRequest1.Execute;

  var JSONValue: TJSONValue;
  var JSONArray: TJSONArray;
  var ArrayElement: TJSONValue;

  RctList       := TList<TRectangle>.Create;
  LblTitleList  := TList<TLabel>.Create;
  LblSourceList := TList<TLabel>.Create;
  LblURLList    := TList<TLabel>.Create;
  MemoList      := TList<TMemo>.Create;

  try
    JSONValue := TJSONObject.ParseJSONValue(RESTResponse1.Content);
    JSONArray := JSONValue.GetValue<TJSONArray>('data');  // articles are stored in the data array in the JSON response

    for ArrayElement in JSONArray do
    begin
      // Create components dynamically
      {$region 'Create news card' }
      RctNewsCard := TRectangle.Create(VertScrollBox1);
      RctNewsCard.Parent := VertScrollBox1;
      RctNewscard.HitTest := False;
      RctNewsCard.Fill.Color := TAlphaColorRec.Ghostwhite;
      RctNewsCard.Fill.Kind  := TBrushKind.Solid;
      RctNewsCard.Stroke.Thickness := 0;
      RctNewsCard.Align := TAlignLayout.Top;
      RctNewsCard.Height := 220;
      RctNewsCard.Width  := 401;
      RctNewsCard.XRadius := 20;
      RctNewsCard.YRadius := 20;
      RctNewsCard.Margins.Top := 5;
      RctNewsCard.Margins.Bottom := 5;
      RctNewsCard.Margins.Left := 10;
      RctNewsCard.Margins.Right := 10;
      RctList.Add(RctNewsCard);  // add to the TList instance
      {$endregion}

      {$region 'create title and summary texts in the News Card' }
      LabelTitle := TLabel.Create(RctNewsCard);
      LabelTitle.Parent := RctNewsCard;
      LabelTitle.Align := TAlignLayout.MostTop;
      LabelTitle.Height := 20;
      LabelTitle.Width  := 371;
      LabelTitle.HitTest := False;
      LabelTitle.AutoSize := True;
      LabelTitle.Font.Size := 16;
      LabelTitle.Margins.Left := 15;
      LabelTitle.Margins.Right := 15;
      LabelTitle.Margins.Top := 5;
      LabelTitle.Margins.Bottom := 5;
      LabelTitle.Text := 'Title: ' + ArrayElement.GetValue<String>('title');
      LblTitleList.Add(LabelTitle);

      MemoDescription := TMemo.Create(RctNewsCard);
      MemoDescription.Parent := RctNewsCard;
      MemoDescription.Align  := TAlignLayout.MostBottom;
      MemoDescription.Height := 100;
      MemoDescription.Width  := 321;
      MemoDescription.HitTest := False;
      MemoDescription.Font.Size := 14;
      MemoDescription.Margins.Left := 10;
      MemoDescription.Margins.Right := 10;
      MemoDescription.Margins.Top := 5;
      MemoDescription.Margins.Bottom := 5;
      MemoDescription.TextSettings.WordWrap := True;
      MemoDescription.Lines.Add(ArrayElement.GetValue<String>('description'));
      MemoList.Add(MemoDescription);

      LabelURL := TLabel.Create(RctNewsCard);
      LabelURL.Parent := RctNewsCard;
      LabelURL.Align := TAlignLayout.Top;
      LabelURL.Height := 17;
      LabelURL.Width  := 381;
      LabelURL.HitTest := False;
      LabelURL.AutoSize := True;
      LabelURL.Font.Size := 10;
      LabelURL.Margins.Left := 15;
      LabelURL.Margins.Right := 15;
      LabelURL.Margins.Top := 5;
      LabelURL.Margins.Bottom := 5;
      LabelURL.Text := 'URL: ' + ArrayElement.GetValue<String>('url');
      LblTitleList.Add(LabelURL);

      LabelSource := TLabel.Create(RctNewsCard);
      LabelSource.Parent := RctNewsCard;
      LabelSource.Align := TAlignLayout.Top;
      LabelSource.Height := 17;
      LabelSource.Width  := 381;
      LabelSource.HitTest := False;
      LabelSource.AutoSize := True;
      LabelSource.Font.Size := 14;
      LabelSource.Margins.Left := 15;
      LabelSource.Margins.Right := 15;
      LabelSource.Margins.Top := 5;
      LabelSource.Margins.Bottom := 5;
      LabelSource.Text := 'Source: ' + ArrayElement.GetValue<String>('source');
      LblTitleList.Add(LabelSource);
      {$endregion}
    end;
  finally
    RctList.Free;
    LblTitleList.Free;
    LblSourceList.Free;
    LblURLList.Free;
    MemoList.Free;
  end;
end;

end.
