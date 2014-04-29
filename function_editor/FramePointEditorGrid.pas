unit FramePointEditorGrid;

(*
FramePointEditorGrid.pas/dfm
----------------------------
Begin: 2005/11/10
Last revision: $Date: 2008/11/25 22:05:57 $ $Author: areeves $
Version number: $Revision: 1.1 $
Project: NAADSM and related applications
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
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    Buttons,
    ExtCtrls,
    Grids,
    Variants,
    Menus,
    FunctionPointers,

    Points
  ;

  type StartGridType = array[0..1] of array of string[20];

  type TFramePointEditorGrid = class( TFrame )
      pnlButtonContainer: TPanel;
      stgPoints: TStringGrid;
      pnlButtons: TPanel;
      btnOK: TBitBtn;
      btnCancel: TBitBtn;
      mnuRowEditor: TPopupMenu;
      itemInsertRow: TMenuItem;
      itemDeleteRow: TMenuItem;

      procedure stgPointsKeyPress( Sender: TObject; var Key: Char );

      procedure btnCancelClick( Sender: TObject );
      procedure btnOKClick( Sender: TObject );

      procedure stgPointsEnter(Sender: TObject);

      procedure stgPointsMouseDown(
        Sender: TObject;
        Button: TMouseButton;
        Shift: TShiftState;
        X, Y: Integer
      );

      procedure stgPointsKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
      procedure stgPointsKeyUp(Sender: TObject; var Key: Word;
        Shift: TShiftState);
      procedure itemInsertRowClick(Sender: TObject);
      procedure itemDeleteRowClick(Sender: TObject);

    protected
    	// For internal use
    	//-----------------
      StartGrid: StartGridType;
      PDF: boolean;
      Posn: longint;
      _myParent: TWinControl;

      _currentColumn: integer;
      _currentRow: integer;

      _generalPoints: T2DPointList;

      _setParentMenuItemsEnabled: TObjFnVoid1Bool;

      // properties
      //-----------
      _prAllPointsEdited: boolean;

      procedure translateUI();

      // Properties
      //-----------
      function getAllPointsEdited(): boolean;

      function getVisible(): boolean;
      procedure setVisible( val: boolean );

    public
      // Construction/initialization/destruction
      //----------------------------------------
    	constructor create( AOwner: TComponent ); override;
			destructor destroy(); override;

      // Used for setting function pointer(s)
      //-------------------------------------
      procedure setSetParentMenuItemsEnabled( fn: TObjFnVoid1Bool );

			// Useful public functions
			//------------------------
      procedure setPoints( pnt: RPointArray );

			{*
			Sets the points in pnt to the points specified on this form.
			}
			procedure resetPoints( var pnt: RPointArray );

			// Properties
			//-----------
      property allPointsEdited: boolean read getAllPointsEdited;
      property visible: boolean read getVisible write setVisible;

  	end
  ;

  const
    DBFRAMEPOINTEDITORGRID: boolean = false; // set to true to enable debugging messages for this unit

implementation

	{$R *.DFM}

  uses
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    ControlUtils,
    I88n
  ;

  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit

//-----------------------------------------------------------------------------
// Local helper functions: I hate these things!
// 	FIX ME: These haven't been reformatted or cleaned up.  They are just
// 	as Mark left them.
//-----------------------------------------------------------------------------
  procedure CheckValidityOfSG(
    sg : TStringGrid; PDF : boolean;
    var ErrorCode : byte;
    var Row : longint);
  var
    r : longint;
    X,Y,LX,LY : double;
    Ok : boolean;
    ec : byte;

  function ConvertStrFloat(s : string;
      var OK : boolean) : double;
    var
      d : double;
    begin
      d := -1;
      Ok := true;
      try
        d := StrToFloat(s);
      except
        Ok := False;
      end;
      Result := d;
    end;

  procedure CheckValues(
      row, RowCount : longint;
      X, LX, Y, LY : double;
      PDF : boolean;
      var ec : byte);
    begin
      if Y < 0 then
        ec := 3 // Y less than 0
      else
      if (PDF and
        ((row = 1) or (row=RowCount-1)) and
        (Y <> 0)) then
          ec := 4  // PDF not beginning and ending on 0
      else
      if row > 1 then
      begin
        OK := LX <= X;
        if not OK then
          ec := 5; // X is less than previous
      end;
    end;

  procedure CheckForZeroArea(
     sg : TStringGrid; var ec : byte);
    var
      Ok : boolean;
      r : longint;
      Y : double;
    begin
      Ok := False;
      for r := 1 to sg.RowCount - 1 do
      begin
        Y := StrToFloat(sg.Cells[1,r]);
        if Y > 0 then
        begin
          Ok := True;
          break;
        end;
      end;
      if not Ok then
        ec := 6;  // Area of 0
    end;

  begin // CheckValidityOfSG
    r := 1; ec := 0; LX := -1; LY := -1;
    repeat
      X := ConvertStrFloat(
          sg.Cells[0,r],OK);
      if Ok then
      begin
        Y := ConvertStrFloat(
          sg.Cells[1,r],OK);
        if OK then
        begin
          CheckValues(r, sg.RowCount,
            X, LX, Y, LY, PDF, ec);
          if ec = 0 then
          begin
            LX := X;
            LY := Y;
            Inc(r);
          end;
        end
        else
          ec := 2; // conversion error Y
      end
      else
        ec := 1; // conversion error X
    until (ec <> 0) or (r >= sg.RowCount);
    if (ec = 0) and PDF then
      CheckForZeroArea(sg, ec);
    ErrorCode := ec;
    Row := r;
  end;


  (*
  function IsDataValid(sg : TStringGrid;
    PDF : boolean) : boolean;
  var
    ErrorCode : byte;
    Row,c : Longint;
    rs, m : string;
    myRect: TGridRect;
  begin // IsDataValid
    CheckValidityOfSG(sg, PDF, ErrorCode, Row);
    rs := IntToStr(Row);
    case ErrorCode of
      1 : m := 'Conversion error for X, row ' + rs;
      2 : m := 'Conversion error for Y, row ' + rs;
      3 : m := 'Y must be 0 or more, row ' + rs;
      4 : m := 'A PDF must begin with Y = 0 and end the same way, row ' + rs;
      5 : m := 'This X value must be equal to or ' +
       'greater than the previous one, row ' + rs;
      6 : m := 'This PDF would not have a defined area.';
    end;
    Result := ErrorCode = 0;
    if (not Result) then
    begin
      // change the position of focus in StringGrid
      if ErrorCode in [1,2,5] then
        c := 0
      else
        c := 1;
      myRect.Left := c;
      myRect.Top := Row;
      myRect.Right := c;
      myRect.Bottom := Row;
      sg.Selection := myRect;
      MessageDlg(m,mtError,[mbOK],0);
    end;
  end;
  *)


  procedure MoveDataToStringGrid( P: RPointArray; sg: TStringGrid );
  var
    r : longint;
  begin
    sg.RowCount := Length(P) + 1;
    for r := Low(P) to High(P) do
    begin
      sg.Cells[0, r + 1] := uiFloatToStr(P[r].X);
      sg.Cells[1, r + 1] := uiFloatToStr(P[r].Y);
    end;
  end;

  procedure MoveStringGridToArray(sg : TStringGrid;
    var StartGrid : StartGridType);
  var
    c,r : longint;
  begin
    SetLength(StartGrid[0], sg.RowCount-1);
    SetLength(StartGrid[1], sg.RowCount-1);
    for c := 0 to 1 do
    for r := 0 to High(StartGrid[c]) do
      StartGrid[c][r] :=
        sg.Cells[c,r+1];
	end;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFramePointEditorGrid.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      _setParentMenuItemsEnabled := nil;

      _myParent := AOwner as TWinControl;
      _prAllPointsEdited := True;

      stgPoints.Cells[0,0] := 'X';
      stgPoints.Cells[1,0] := 'Y';
      stgPoints.DefaultColWidth := self.Width div 2;

      _generalPoints := T2DPointList.create();

      _currentColumn := -1;
      _currentRow := -1;
    end
  ;
    
    
  procedure TFramePointEditorGrid.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:50:37 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/function_editor/FramePointEditorGrid.dfm
      // File date: Thu Nov 10 10:52:48 2005

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          btnOK.Caption := tr( 'OK' );
          btnCancel.Caption := tr( 'Cancel' );
        end
      ;

    end
  ;
    
  
  destructor TFramePointEditorGrid.destroy();
  	begin
  		// Not sure what these do here: copied over from Mark's DestroyForm event.
      StartGrid[0] := nil;
      StartGrid[1] := nil;

      _generalPoints.Free();
      
  		inherited destroy();
  	end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
//  Useful public functions
//-----------------------------------------------------------------------------
  procedure TFramePointEditorGrid.setPoints( pnt: RPointArray );
    var
      i: integer;
    begin
      _generalPoints.Assign( pnt );

      stgPoints.RowCount := _generalPoints.Count + 1; // Don't forget the header row

      for i := 0 to _generalPoints.Count - 1 do
        begin
          stgPoints.Cells[0, i+1] := uiFloatToStr( _generalPoints.at(i).x );
          stgPoints.Cells[1, i+1] := uiFloatToStr( _generalPoints.at(i).y );
        end
      ;
    end
  ;


	procedure TFramePointEditorGrid.resetPoints( var pnt: RPointArray );
  	var
    	i: longint;
    begin
      // Move data from string grid into point array
      for i := Low( pnt ) to High( pnt ) do
        begin
          pnt[i].X := StrToFloat( stgPoints.Cells[0, i + 1] );
          pnt[i].Y := StrToFloat( stgPoints.Cells[1, i + 1] );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Function pointers
//-----------------------------------------------------------------------------
  procedure TFramePointEditorGrid.setSetParentMenuItemsEnabled( fn: TObjFnVoid1Bool );
    begin
      _setParentMenuItemsEnabled := fn;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Event handlers
//-----------------------------------------------------------------------------
  procedure TFramePointEditorGrid.btnOKClick(Sender: TObject);
    begin
      dbcout( 'OK clicked', DBSHOWMSG );

      btnOK.Enabled := false;
      btnCancel.Enabled := false;

      if( nil <> @_setParentMenuitemsEnabled ) then _setParentMenuItemsEnabled( true );
    end
  ;


  procedure TFramePointEditorGrid.btnCancelClick(Sender: TObject);
    begin
      dbcout( 'Cancel clicked', DBSHOWMSG );

      btnOK.Enabled := false;
      btnCancel.Enabled := false;

      if( nil <> @_setParentMenuitemsEnabled ) then _setParentMenuItemsEnabled( true );
    end
  ;

  procedure TFramePointEditorGrid.stgPointsEnter(Sender: TObject);
    begin
      dbcout( 'Entered!', DBSHOWMSG );
      btnOK.Enabled := true;
      btnCancel.Enabled := true;

      dbcout( '_setParentMenuitemsEnabled is nil: ' + uiBoolToText( nil = @_setParentMenuitemsEnabled ), DBSHOWMSG );

      if( nil <> @_setParentMenuitemsEnabled ) then _setParentMenuItemsEnabled( false );

      (*
      try
        stgPoints.MouseToCell( x, y, c, r );

        if( 0 < r ) then
          begin
            btnOK.Enabled := true;
            btnCancel.Enabled := true;
          end
        ;
      except
        // The cursor is probably not over a cell.
        // Do nothing and fail silently.
      end;
      *)
    end
  ;


  procedure TFramePointEditorGrid.stgPointsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    inherited;
    dbcout2( 'Key down: ' + intToStr( Key ) );
  end
  ;


  procedure TFramePointEditorGrid.stgPointsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      dbcout2( 'Key up: ' + intToStr( Key ) );
    end
  ;


  procedure TFramePointEditorGrid.stgPointsKeyPress(Sender: TObject; var Key: Char);
    begin
      // Only allow number keys, -, decimal point, and backspace
      if not (Key in ['0'..'9', '-', decPt(), CHAR_BACKSPACE ]) then
        Key := ^@
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
	function TFramePointEditorGrid.getAllPointsEdited(): boolean; begin result := _prAllPointsEdited; end;


  function TFramePointEditorGrid.getVisible(): boolean;
    begin
      result := inherited visible;
    end
  ;


  procedure TFramePointEditorGrid.setVisible( val: boolean );
    begin
      self.Width := _myParent.Width;
      pnlButtonContainer.Width := _myParent.Width;

      horizCenterInside( pnlButtons, pnlButtonContainer );

      stgPoints.DefaultColWidth := self.Width div 2;
      stgPoints.ColWidths[0] := self.Width div 2 - 10;
      stgPoints.ColWidths[1] := self.Width div 2 - 10;
      
      inherited visible := val;
    end
  ;
//-----------------------------------------------------------------------------




  procedure TFramePointEditorGrid.stgPointsMouseDown(
        Sender: TObject;
        Button: TMouseButton;
        Shift: TShiftState;
        X, Y: Integer
      );
    var
      clientPoint: TPoint;
      screenPoint: TPoint;
    begin
      clientPoint.X := x;
      clientPoint.Y := y;

      screenPoint := clientToScreen( clientPoint );
      
      try
        stgPoints.MouseToCell( x, y, _currentColumn, _currentRow );
      except
        _currentColumn := -1;
        _currentRow := -1;
      end;

      if( mbRight = Button ) then
        begin
          dbcout2( 'Right button! c = %d, r = %d', [_currentColumn, _currentRow] );
          if( 0 <> _currentRow ) then
            mnuRowEditor.Popup( screenPoint.x, screenPoint.y + 5 )
          ;
        end
      ;

      dbcout( 'c = %d, r = %d', [_currentColumn, _currentRow], DBSHOWMSG );
    end
  ;



  procedure TFramePointEditorGrid.itemInsertRowClick(Sender: TObject);
    var
      colX: TStringList;
      ColY: TStringList;
      i: integer;
      x, y: string;
    begin
      colX := TStringList.Create();
      colY := TStringList.Create();

      for i := 0 to stgPoints.RowCount - 1 do
        begin
          colX.Add( stgPoints.Cells[0,i] );
          colY.Add( stgPoints.Cells[1,i] );
        end
      ;
      dbcout2( 'Current row: ' + intToStr( _currentRow ) );

      // Add new values to the end of the list
      if( -1 = _currentRow ) then
        begin
          colX.Add( uiFloatToStr( myStrToFloat( colX[ colX.Count - 1 ] ) + 1 ) );
          colY.Add( colY[ colY.count - 1 ] );
        end
      else // insert in the appropriate place
        begin
          if( 1 < _currentRow ) then
            begin
              x := uiFloatToStr( ( myStrToFloat( colX[ _currentRow - 1 ] ) + myStrToFloat( colX[ _currentRow ] ) ) / 2 );
              y := uiFloatToStr( ( myStrToFloat( colY[ _currentRow - 1 ] ) + myStrToFloat( colY[ _currentRow ] ) ) / 2 );
            end
          else
            begin
              x := colX[ 1 ];
              y := colY[ 1 ];
            end
          ;

          colX.Insert( _currentRow, x );
          colY.Insert( _currentRow, y );
        end
      ;

      stgPoints.RowCount := stgPoints.RowCount + 1;
      for i := 0 to stgPoints.RowCount - 1 do
        begin
          stgPoints.Cells[ 0, i ] := colX[ i ];
          stgPoints.Cells[ 1, i ] := colY[ i ];
        end
      ;

      itemDeleteRow.Enabled := ( 3 < stgPoints.RowCount );

      freeAndNil( colX );
      freeAndNil( colY );
    end
  ;



  procedure TFramePointEditorGrid.itemDeleteRowClick(Sender: TObject);
    var
      colX: TStringList;
      ColY: TStringList;
      i: integer;
    begin
      colX := TStringList.Create();
      colY := TStringList.Create();

      for i := 0 to stgPoints.RowCount - 1 do
        begin
          colX.Add( stgPoints.Cells[0,i] );
          colY.Add( stgPoints.Cells[1,i] );
        end
      ;

      if( -1 = _currentRow ) then
        begin
          colX.Delete( colX.Count - 1 );
          colY.Delete( colY.Count - 1 );
        end
      else
        begin
          colX.Delete( _currentRow );
          colY.Delete( _currentRow );
        end
      ;

      stgPoints.RowCount := stgPoints.RowCount - 1;
      for i := 0 to stgPoints.RowCount - 1 do
        begin
          stgPoints.Cells[ 0, i ] := colX[ i ];
          stgPoints.Cells[ 1, i ] := colY[ i ];
        end
      ;

      itemDeleteRow.Enabled := ( 3 < stgPoints.RowCount );

      freeAndNil( colX );
      freeAndNil( colY );
    end
  ;

end.
