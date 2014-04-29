unit FrameArrayHistogram;

(*
FrameArrayHistogram.pas/dfm
----------------------------
Begin: 2005/08/01
Last revision: $Date: 2008/11/25 22:05:57 $ $Author: areeves $
Version number: $Revision: 1.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    REEdit,
    TeEngine,
    Series,
    TeeProcs,
    Chart,
    Buttons,
    ExtCtrls,
    Math,

    QVectors,

		FrameChartBase,

    RegExpDefs
  ;

  type TPtr = procedure(Sender: TObject) of object;

  type TSetBinNumberPtr = procedure( sender: TComponent; const nBins: integer ) of object;

  type TFrameArrayHistogram = class( TFrameChartBase )
      chtHistogram: TChart;
      serHistogram: TBarSeries;

      pnlControls: TPanel;
      lblHistoBins: TLabel;
      rleHistoBins: TREEdit;
      btnAccept: TBitBtn;
      btnCancel: TBitBtn;

      procedure rleHistoBinsEnter(Sender: TObject);
      procedure btnAcceptClick(Sender: TObject);
      procedure btnCancelClick(Sender: TObject);
      procedure rleHistoBinsKeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );
      procedure rleHistoBinsClick(Sender: TObject);

    protected
      _arr: TQDoubleVector;
      _numberOfBins: integer;
      _myForm: TForm;

      _lineEditEntered: boolean;

      procedure translateUI();
      procedure translateUIManual();

      procedure populateChart();
      procedure setNumberOfBins( const val: integer );
      
    public
      _entry: integer;
      entryPtr: ^integer;
      btnAcceptClickPtr: TPtr;
      btnCancelClickPtr: TPtr;
      rleHistoBinsEnterPtr: TPtr;

      setBinNumberPtr: TSetBinNumberPtr;

      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      procedure populateFromArray( src: TQDoubleVector );
      procedure setTitle( titleStr: string );

      property nHistogramBins: integer read _numberOfBins write setNumberOfBins;
       
    end
  ;


implementation

{$R *.dfm}

  uses
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    MyDialogs,
    I88n
  ;


  const
    DBFRAMEARRAYHISTOGRAM: boolean = false; // Set to true to enable debugging messages for this unit


  constructor TFrameArrayHistogram.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      if( AOwner is TForm ) then
        _myForm := AOwner as TForm
      else
        _myForm := nil
      ;

      _arr := nil;

      setBinNumberPtr := nil;

      rleHistoBins.InputExpression := RE_INTEGER_INPUT;
      _numberOfBins := -1;
      _lineEditEntered := false;
    end
  ;


  procedure TFrameArrayHistogram.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Thu Feb 28 16:52:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/FrameArrayHistogram.dfm
      // File date: Wed Feb 27 08:41:02 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblHistoBins.Caption := tr( 'Number of histogram bins:' );
        end
      ;

      // Set TChart properties
      with self do
        begin
          chtHistogram.BottomAxis.Title.Caption := tr( 'Output value' );
          chtHistogram.LeftAxis.Title.Caption := tr( 'Frequency' );
          chtHistogram.Title.Text.Strings[0] := tr( 'TChart' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      // Otherwise, this function will be empty:
      translateUIManual();
    end
  ;


  procedure TFrameArrayHistogram.translateUIManual();
    begin
    end
  ;


  destructor TFrameArrayHistogram.destroy();
    begin
      if( nil <> _arr ) then _arr.Free(); 
      inherited destroy();
    end
  ;


  procedure TFrameArrayHistogram.populateFromArray( src: TQDoubleVector );
    begin
      // Create a copy of the array: The original should not be sorted.
      if( nil <> _arr ) then
        freeAndNil( _arr )
      ;
      _arr := TQDoubleVector.create( src );
      _arr.Sort();

      populateChart();
    end
  ;


  procedure TFrameArrayHistogram.setNumberOfBins( const val: integer );
    begin
      if( 0 < val ) then
        _numberOfBins := val
      else
        _numberOfBins := 50
      ;

      rleHistoBins.Text := intToStr( _numberOfBins );
      populateChart();
    end
  ;


  procedure TFrameArrayHistogram.populateChart();
    var
      min, max: double;
      range: double;

      currentBin: integer;
      binMidpoint: double;
      binInterval: double;
      nextIntervalStart: double;
      binFreq: integer;
      arrIndex: integer;
    begin
      serHistogram.Clear();

      if( nil = _arr ) then
        exit
      ;
      
      min := _arr[0];
      max := _arr[_arr.Count-1];

      if( -1 = _numberOfBins ) then
        begin
          _numberOfBins := 50;

          rleHistoBins.Text := intToStr( _numberOfBins );
        end
      ;

      range := max - min;
      dbcout( 'Number of bins: ' + intToStr( _numberOfBins ), DBFRAMEARRAYHISTOGRAM );
      binInterval := range / _numberOfBins;
      arrIndex := 0;

      if( DBFRAMEARRAYHISTOGRAM ) then
        begin
          dbcout( 'Count: ' + intToStr( _arr.Count ), DBFRAMEARRAYHISTOGRAM );
          dbcout( 'min: ' + uiFloatToStr( min ), DBFRAMEARRAYHISTOGRAM );
          dbcout( 'max: ' + uiFloatToStr( max ), DBFRAMEARRAYHISTOGRAM );
          dbcout( 'range: ' + uiFloatToStr( range ), DBFRAMEARRAYHISTOGRAM );
          dbcout( 'Number of bins: ' + intToStr( _numberOfBins ), DBFRAMEARRAYHISTOGRAM );
          dbcout( 'binInterval: ' + uiFloatToStr( binInterval ), DBFRAMEARRAYHISTOGRAM );
        end
      ;

      for currentBin := 1 to _numberOfBins do
        begin
          nextIntervalStart := min + ( currentBin * binInterval ); // upper interval value
          binFreq := 0;

          while( ( arrIndex < _arr.Count ) and ( nextIntervalStart >= _arr[arrIndex] ) ) do
            begin
              inc( binFreq );
              inc( arrIndex );
            end
          ;

						//Add To the series object
						binMidpoint := min + ((currentBin - 1) * binInterval) + ( binInterval/2 ); 

            if( DBFRAMEARRAYHISTOGRAM ) then
              begin
                dbcout( 'binMidPoint: ' + uiFloatToStr( binMidpoint ), DBFRAMEARRAYHISTOGRAM );
                dbcout( 'binFreq: ' + intToStr( binFreq ), DBFRAMEARRAYHISTOGRAM );
              end
            ;
            
						serHistogram.AddXY( binMidpoint, binFreq );
        end
      ;

      // The following IF statements correct for a known bug in TChart
      // See http://www.teechart.net/support/modules.php?name=Newsgroups&file=article&id=924&group=steema.public.teechart6.delphi
      if( serHistogram.YValues.MaxValue = serHistogram.YValues.MinValue ) then
        serHistogram.GetVertAxis.SetMinMax( serHistogram.YValues.MinValue - 1.0, serHistogram.YValues.MinValue + 1.0 )
      else
        serHistogram.GetVertAxis.Automatic := true
      ;

      if( serHistogram.XValues.MaxValue = serHistogram.XValues.MinValue ) then
        serHistogram.GetHorizAxis.SetMinMax( serHistogram.XValues.MinValue - 1.0, serHistogram.XValues.MinValue + 1.0 )
      else
        serHistogram.GetHorizAxis.Automatic := true
      ;

      serHistogram.GetVertAxis.SetMinMax( 0.0, serHistogram.YValues.MaxValue + ( 0.05 * serHistogram.YValues.MaxValue ) );
      //serHistogram.GetHorizAxis.SetMinMax( serHistogram.XValues.MinValue - binInterval, serHistogram.XValues.MaxValue + binInterval );
    end
  ;


  procedure TFrameArrayHistogram.setTitle( titleStr: string );
    begin
      chtHistogram.Title.Text[0] := titleStr;
    end
  ;


  procedure TFrameArrayHistogram.rleHistoBinsEnter(Sender: TObject);
    begin
      btnAccept.Enabled := true;
      btnCancel.enabled := true;

      rleHistoBins.SelectAll();

      (*
      if(sender = self.rleHistoBins) then
        begin
          if(nil <> @rleHistoBinsEnterPtr) then
          begin
            rleHistoBinsEnterPtr(Sender);
          end
          ;
        end
      ;
      *)
    end
  ;

  
  procedure TFrameArrayHistogram.rleHistoBinsClick(Sender: TObject);
    begin
      if( not( _lineEditEntered ) ) then
        begin
          _lineEditEntered := true;
          rleHistoBins.SelectAll();
        end
      ;
    end
  ;


  procedure TFrameArrayHistogram.btnAcceptClick(Sender: TObject);
    begin
      if(sender = self.btnAccept) then
        begin
          _entry := strToInt( rleHistoBins.Text );

          if(nil <> @btnAcceptClickPtr) then
            begin
              entryPtr^ :=  _entry;
              btnAcceptClickPtr(Sender);
            end
          ;
        end
      ;
      if( not( 0 < _entry ) ) then
        begin
          msgOK( tr( 'Please enter a positive number of histogram bins.' ), tr( 'Invalid entry' ), IMGWarning, _myForm );
          rleHistoBins.SetFocus();
          btnAccept.Enabled := true;
          btnCancel.Enabled := true;
        end
      else
        begin
          _numberOfBins := _entry;
          btnAccept.Enabled := false;
          btnCancel.Enabled := false;
          rleHistoBins.Text := intToStr(_entry);

          populateChart();

          if( nil <> @setBinNumberPtr ) then
            setBinNumberPtr( self, _numberOfBins )
          ;

          _lineEditEntered := false;

          chtHistogram.SetFocus();
        end
      ;
    end
    ;


  procedure TFrameArrayHistogram.btnCancelClick(Sender: TObject);
    begin
      if(sender = self.btnCancel) then
      begin
        if(nil <> @btnCancelClickPtr) then
        begin
          btnCancelClickPtr(Sender);
        end
        ;
      end
      ;
      rleHistoBins.Text := intToStr( _numberOfBins );
      btnAccept.Enabled := false;
      btnCancel.Enabled := false;
      chtHistogram.SetFocus();
    end
  ;


  procedure TFrameArrayHistogram.rleHistoBinsKeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );
    begin
      dbcout( 'Key up!' + intToStr( ord( key ) ), DBFRAMEARRAYHISTOGRAM );
      
      if( 27 = key ) then
        btnCancelClick( btnCancel )
      else if( 13 = key ) then
        btnAcceptClick( btnAccept )
      ;
    end
  ;


end.
