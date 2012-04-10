program GenaratorDataset;

uses
  Forms,
  uExportDataset in 'uExportDataset.pas' {Frm_ExportDataset},
  uDMGenerator in 'uDMGenerator.pas' {DMGenerator: TDataModule},
  uTableForExport in 'uTableForExport.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDMGenerator, DMGenerator);
  Application.CreateForm(TFrm_ExportDataset, Frm_ExportDataset);
  Application.Run;
end.
