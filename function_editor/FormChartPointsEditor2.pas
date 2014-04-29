unit FormChartPointsEditor2;

(*
FormChartPointsEditor2.pas/dfm
-------------------------------
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
    // standard includes
    Windows,
    SysUtils,
    Variants,
    Forms,
    Clipbrd,
    Menus,
    ActnList,
    XPStyleActnCtrls,
    ActnMan,
    ToolWin,
    ActnCtrls,                                         
    ActnMenus,
    ImgList,
    Classes,
    Controls,
    Dialogs,
    StdCtrls,
    ExtCtrls,
		Buttons,
		
    // Chart controls
    TeEngine,
    Series,
    TeeProcs,
    Chart,

    // Data
    Points,
    ChartFunction,
    RelFunction,
    ProbDensityFunctions,

    // Widgets
    FrameFunctionParams2,
    FrameChartPointsEditor,
    FrameChartBase
  ;


  type ARRIntervalArea = array of double;

  type TFormChartPointsEditor2 = class( TForm )
      dlgPrint: TPrintDialog;
      dlgExportCSV: TSaveDialog;
      dlgImportCSV: TOpenDialog;
      dlgSaveWMF: TSaveDialog;

      pnlControls: TPanel;

      pnlMenu: TPanel;
      ActionMainMenuBar1: TActionMainMenuBar;

      pnlButtons: TPanel;
      btnSaveChart: TBitBtn;
      btnCopyChart: TBitBtn;
      btnPrintChart: TBitBtn;
      btnAcceptChanges: TBitBtn;
      btnCancelChanges: TBitBtn;
      pnlDecorator1: TPanel;
      pnlDecorator2: TPanel;

      pnlBody: TPanel;
      fraChartPointsEditor: TFrameChartPointsEditor;

      pnlControlsBottom: TPanel;
      btnAcceptChanges2: TBitBtn;
      btnCancelChanges2: TBitBtn;

      pnlTitle: TPanel;
      lblTitle: TLabel;
      leTitle: TEdit;
      pnlDecorator4: TPanel;
      pnlDecorator3: TPanel;

      pnlParams: TPanel;
      fraFunctionParams: TFrameFunctionParams2;

      ActionManager1: TActionManager;
      ImageList1: TImageList;

      actionPrintChart: TAction;
      actionSaveChart: TAction;
      actionCopyChart: TAction;

      actionSavePoints: TAction;
      actionCopyPoints: TAction;
      actionPastePoints: TAction;
      actionImportPoints: TAction;

      actionFinish: TAction;
      ActionClose: TAction;

      // Help menu items
      //-----------------
      ActionHelp: TAction;

      procedure FormCreate(Sender: TObject);
      

      // Import/export
      //---------------
      procedure savePoints( Sender: TObject );
      procedure copyPoints( Sender: TObject );
      procedure importPoints( Sender: TObject );

      // Applying changes
      //-----------------
      procedure ActionSaveExitExecute( sender: TObject );
      procedure ActionExitExecute(Sender: TObject);

      // Chart utilities
      //----------------
      procedure saveChartToWMF( sender: TObject );
      procedure printChart( sender: TObject );
      procedure copyChartToClipboard( sender: TObject );

      // Help
      //-----
      procedure ActionHelpExecute(Sender: TObject);

    protected
      _title: string;
      _isReadOnly: boolean;

      _saveChanges: boolean;

      procedure translateUI();
      procedure translateUIManual();

      // Functions for import of points from clipboard or text file
      //------------------------------------------------------------
      { Checks the validity of imported points. }
      function ProcessPointsOK( var P : RPointArray ): boolean;


      // Functions for internal use
      //----------------------------
      procedure initialize( AOwner: TComponent; readOnly: boolean );

      procedure setAxisUnits( xAxisUnits, yAxisUnits: TChartUnitType );

      procedure updateFunctionDisplay( sender: TObject; fn: TChartFunction );
      procedure setMenuItemsEnabled( val: boolean );


      // Properties
      //-----------
      procedure setIsReadOnly( const val: boolean );
      function getIsReadOnly(): boolean;

      procedure setTitle( val: string );
      function getTitle(): string;

      function getSaveChanges(): boolean;

      procedure setChartType( const val: TChartType );
      function getChartType(): TChartType;

    public
    	// Construction/initialization/destruction
      //-----------------------------------------
      { Used to create a new default PDF or relational function. }
      constructor create(
        AOwner: TComponent;
        const chartType: TChartType;
        const xAxisUnits: TChartUnitType = UnitsUnknown;
        const yAxisUnits: TChartUnitType = UnitsUnknown;
        const unitsLocked: boolean = true;
        const minY: double = 0.0;
        const maxY: double = 0.0;
        const title: string = '';
        const titleLocked: boolean = true;
        const readOnly: boolean = false
      ); reintroduce; overload;


      { Used with an existing PDF or relational function. }
      constructor create(
        AOwner: TComponent;
        const chart: TChartFunction;
        const minY: double = 0.0;
        const maxY: double = 0.0;
        const readOnly: boolean = false
      ); reintroduce; overload;


      { Creates a completely empty instance.  setFunction must be used, once it exists. }
      constructor create( AOwner: TComponent ); overload; override;

      { Sets the form contents to the specified chart. }
      procedure setFunction( chart: TChartFunction );


      // Useful public functions
      //------------------------
      { Generates a new chart function based on displayed parameters. }
      function createFunction(): TChartFunction;


      property chartType: TChartType read getChartType write setChartType;
      property title: string read getTitle write setTitle;
      property isReadOnly: boolean read getIsReadOnly write setIsReadOnly;

      property changesSaved: boolean read getSaveChanges;
    end
  ;


implementation

  {$R *.dfm}
  
  uses
    Math,
    StrUtils,

    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    CsvParser,
    ControlUtils,
    MyDialogs,
    I88n
  ;

  const
  	DBFORMCHARTPOINTSEDITOR: boolean = false; // set to true to enable debugging messages for this unit.


//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
	{*
	This form of the constructor is used to create a brand new piecewise PDF or relational function.
	Note that no points exist yet.  A default function of the specified type is created and 
	immediately shown.

	Chart type must be specified (CTPdf for a pdf, CTRel for a relational function)
	Units must also be specified.
	}
  constructor TFormChartPointsEditor2.create(
      AOwner: TComponent;
      const chartType: TChartType;
      const xAxisUnits: TChartUnitType = UnitsUnknown;
      const yAxisUnits: TChartUnitType = UnitsUnknown;
      const unitsLocked: boolean = true;
      const minY: double = 0.0;
      const maxY: double = 0.0;
      const title: string = '';
      const titleLocked: boolean = true;
      const readOnly: boolean = false
      );
    begin
    	inherited create( AOwner );
      translateUI();
      
      initialize( AOwner, readOnly );

      setTitle( title );
      leTitle.Enabled := not titleLocked;

      setAxisUnits( xAxisUnits, yAxisUnits );

      fraFunctionParams.unitsLocked := unitsLocked;

      fraChartPointsEditor.minY := minY;
      fraChartPointsEditor.maxY := maxY;
      fraFunctionParams.minY := minY;
      fraFunctionParams.maxY := maxY;

      setChartType( chartType );
    end
  ;

 
	{*
	This creates a completely empty instance of TFrameChartPointsEditor.  This form of the constructor is
	useful when a chart will be specified later with setFunction.  No function is shown:
	once a function has been set, it must be explicitly shown with a call to setFunction.

	(This constructor is not used in NAADSM)
	}
  constructor TFormChartPointsEditor2.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      initialize( AOwner, true );
      translateUI();

      setTitle( '' );

      setChartType( CTUnspecified );

      // Until there is a chart, hide the chart-related menu items and buttons.
      setMenuItemsEnabled( false );
    end
  ;



	{*
	This form of the constructor could be used to set up an existing PDF or relational function.

	Units are specified by the instance of the chart.

	(This form of the constructor is not used in SpreadModel: the PDF editor widgets use
	arrays of points, from which they create new instances of TPdfPiecewise, rather than use
	existing instances.)
	}
  constructor TFormChartPointsEditor2.create(
        AOwner: TComponent;
        const chart: TChartFunction;
        const minY: double = 0.0;
        const maxY: double = 0.0;
        const readOnly: boolean = false
      );
    begin
      inherited create( AOwner );
      initialize( AOwner, readOnly );
      translateUI();

      setTitle( chart.name );

      fraChartPointsEditor.minY := minY;
      fraChartPointsEditor.maxY := maxY;
      fraFunctionParams.minY := minY;
      fraFunctionParams.maxY := maxY;

      setFunction( chart );
    end
  ;


  procedure TFormChartPointsEditor2.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Thu Feb 28 21:37:05 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/function_editor/FormChartPointsEditor2.dfm
      // File date: Tue Feb 26 19:38:16 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Chart' );
          ActionMainMenuBar1.Caption := tr( 'ActionMainMenuBar1' );
          btnSaveChart.Hint := tr( 'Save chart image to file' );
          btnCopyChart.Hint := tr( 'Copy chart image to clipboard' );
          btnPrintChart.Hint := tr( 'Print chart image' );
          btnAcceptChanges.Hint := tr( 'Save changes and exit' );
          btnCancelChanges.Hint := tr( 'Exit without saving changes' );
          lblTitle.Caption := tr( 'Function name:' );
          leTitle.Text := tr( 'leTitle' );
          btnAcceptChanges2.Caption := tr( 'Save' );
          btnAcceptChanges2.Hint := tr( 'Save changes and exit' );
          btnCancelChanges2.Caption := tr( 'Cancel' );
          btnCancelChanges2.Hint := tr( 'Exit without saving changes' );
          ActionHelp.Caption := tr( '&Help' );
          ActionHelp.Hint := tr( 'Show help screen' );
          ActionClose.Caption := tr( 'E&xit without saving changes to function' );
          actionFinish.Caption := tr( '&Save changes to function and exit' );
          actionPrintChart.Caption := tr( '&Print chart image...' );
          actionSaveChart.Caption := tr( 'S&ave chart image...' );
          actionCopyChart.Caption := tr( 'C&opy chart image to clipboard' );
          actionSavePoints.Caption := tr( '&Export points to file...' );
          actionCopyPoints.Caption := tr( '&Copy points to clipboard' );
          actionPastePoints.Caption := tr( '&Import points from clipboard' );
          actionImportPoints.Caption := tr( '&Import points from file...' );
          dlgExportCSV.Filter := tr( 'Comma delimited file (*.csv)|*.csv|All files (*.*)|*.*' );
          dlgSaveWMF.Filter := tr( 'Windows Metafile (*.wmf)|*.wmf|All files (*.*)|*.*' );
          dlgImportCSV.Filter := tr( 'Text files (*.csv, *.txt)|*.csv; *.txt|All files (*.*)|*.*' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFormChartPointsEditor2.translateUIManual();
    begin
      self.ActionMainMenuBar1.ActionManager.ActionBars[0].Items[0].Caption := tr( '&File' );
      self.ActionMainMenuBar1.ActionManager.ActionBars[0].Items[1].Caption := tr( '&Edit' );
    end
  ;



 	procedure TFormChartPointsEditor2.FormCreate(Sender: TObject);
 		begin
      if Screen.PixelsPerInch <> 96 then
        begin
          ScaleBy( Screen.PixelsPerInch, 96 );
				  self.width := round( self.width * ( screen.pixelsPerInch / 96 ) );
				  self.height := round( self.height * ( screen.pixelsPerInch / 96 ) );
        end
      ;
 		end
	;


  procedure TFormChartPointsEditor2.initialize( AOwner: TComponent; readOnly: boolean );
    begin
      // Deal with form scaling
      //-----------------------
      Assert(not Scaled, 'You should set Scaled property of Form to False!');
      // Set this value to the PPI AT WHICH THE FORM WAS ORIGINALLY DESIGNED!!
      self.PixelsPerInch := 96;
      // FormCreate() will handle the rest.

      setIsReadOnly( readOnly );
      fraFunctionParams.setUpdateParams( self.updateFunctionDisplay );
      fraFunctionParams.setSetParentMenuItemsEnabled( self.setMenuItemsEnabled );
      fraChartPointsEditor.setUpdateParams( self.updateFunctionDisplay );
    end
  ;


  procedure TFormChartPointsEditor2.setFunction( chart: TChartFunction );
    var
      errMsg: string;
    begin
      dbcout( '@@@ TFormChartPointsEditor2.setFunction', DBFORMCHARTPOINTSEDITOR );

      setChartType( chart.chartType );

      setAxisUnits( chart.xUnits, chart.yUnits );

      fraFunctionParams.editEnabled := false; // This prevents an obsolete message box from popping up

      case chartType of
        CTPdf, CTRel:
          begin
            fraFunctionParams.copyChart( chart, true );

            if( chart is TPdfPoint ) then
              fraChartPointsEditor.hideChart( tr( 'Preview is not available for fixed values.' ) )
            else
              begin
                fraChartPointsEditor.copyChart( chart );

                if( chart.validate( @errMsg ) ) then
                  fraChartPointsEditor.showChart()
                else
                  fraChartPointsEditor.hideChart( tr( 'Function parameters are not valid:' ) + endl + errMsg )
                ;
              end
            ;
            
            setMenuItemsEnabled( true );
          end
        ;
        else
          raise exception.Create( 'TFormChartPointsEditor2.create doesn''t support this chart type' )
        ;
      end;
      
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Creating a chart function (the output of this form!)
//-----------------------------------------------------------------------------
  function TFormChartPointsEditor2.createFunction(): TChartFunction;
    begin
      dbcout2( '** Begin TFormChartPointsEditor2.createFunction' );

      case chartType of
        CTPdf: result := fraFunctionParams.createPdf();
        CTRel: result := fraFunctionparams.createRel();
        else result := nil;
      end;

      if( nil <> result ) then
        result.name := trim( leTitle.Text )
      ;

      dbcout2( '** Done with TFormChartPointsEditor2.createFunction' );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Internal display
//-----------------------------------------------------------------------------
  procedure TFormChartPointsEditor2.updateFunctionDisplay( sender: TObject; fn: TChartFunction );
    var
      msg: string;

      tmpPoints: RPointArray;
    begin
      dbcout( 'TFormChartPointsEditor2.updateFunctionDisplay', DBFORMCHARTPOINTSEDITOR );

      if( sender is TFrameFunctionParams2 ) then
        begin
          if( nil <> fn ) then
            begin
              if( fn.validate( @msg ) ) then
                begin
                  setMenuItemsEnabled( true );

                  if( fn is TPdfPoint ) then
                    fraChartPointsEditor.hideChart( tr( 'Preview is not available for fixed values.' ) )
                  else
                    begin
                      fraChartPointsEditor.copyChart( fn );
                      fraChartPointsEditor.showChart();
                      if( ( fn is TPdfPiecewise ) or ( fn is TRelFunction ) ) then
                        fraChartPointsEditor.isReadOnly := false
                      else
                        fraChartPointsEditor.isReadOnly := true
                      ;
                    end
                  ;
                end
              else
                begin
                  setMenuItemsEnabled( false );
                  dbcout( 'Function parameters are not valid:' + endl + msg, DBFORMCHARTPOINTSEDITOR );
                  fraChartPointsEditor.hideChart( tr( 'Function parameters are not valid:' ) + endl + msg );
                end
              ;
            end
          else
            begin
              setMenuItemsEnabled( false );
              fraChartPointsEditor.hideChart( tr( 'Function is unspecified.' ) );
            end
          ;
        end
      else if( sender is TFrameChartPointsEditor ) then
        begin
          tmpPoints := fraChartPointsEditor.createRPointArray();
          fraFunctionParams.setPoints( tmpPoints );
          setLength( tmpPoints, 0 );
        end
      ;
    end
  ;


  procedure TFormChartPointsEditor2.setMenuItemsEnabled( val: boolean );
    var
      fn: TChartFunction;
    begin
      if( false = val ) then
        val := false
      else
        begin
          fn := createFunction();
          val := fn.isValid;
          fn.debug();
          fn.Free();
        end
      ;

      dbcout( 'setMenuItemsEnabled called', DBFORMCHARTPOINTSEDITOR );

      actionPrintChart.Enabled := val;
      actionSaveChart.Enabled := val;
      actionCopyChart.Enabled := val;

      if( val and not( isReadOnly ) ) then
        dbcout( '++ Enabling actionFinish in setMenuItemsEnabled', DBFORMCHARTPOINTSEDITOR )
      else
        dbcout( '-- Disabling actionFinish in setMenuItemsEnabled', DBFORMCHARTPOINTSEDITOR )
      ;

      actionFinish.Enabled := val and not( isReadOnly );

      btnPrintChart.Enabled := val;
      btnSaveChart.Enabled := val;
      btnCopyChart.Enabled := val;
      btnAcceptChanges.Enabled := val and not( isReadOnly );
      btnAcceptChanges2.Enabled := val and not( isReadOnly );

      actionCopyPoints.Enabled := val;
      actionSavePoints.Enabled := val;

      actionPastePoints.Enabled := not( isReadOnly );
      actionImportPoints.Enabled := not( isReadOnly );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  procedure TFormChartPointsEditor2.setTitle( val: string );
  	begin
      if( 0 = length( val ) ) then val := tr( '(Unnamed function)' );

    	_title := val;
      leTitle.Text := val;
    end
  ;

  function TFormChartPointsEditor2.getTitle(): string; begin Result := _title; end;

  function TFormChartPointsEditor2.getIsReadOnly(): boolean; begin result := _isReadOnly; end;

  procedure TFormChartPointsEditor2.setIsReadOnly( const val: boolean );
    begin
      _isReadOnly := val;

      if( not( val ) ) then dbcout( '-- Enabling actionFinish', DBFORMCHARTPOINTSEDITOR );
      actionFinish.Enabled := ( not val );
      actionPastePoints.Enabled := ( not val );
      actionImportPoints.Enabled := ( not val );

      if( val ) then
        begin
          //lblChanged.caption := tr( '(Locked)' ) + ' ';
          actionClose.Caption := tr( 'E&xit' );
        end
      else
        begin
          //lblChanged.caption := '';
          actionClose.Caption := tr( 'E&xit without saving changes' );
        end
      ;

      // ChartPointsEditor is always read-only, unless editing a piecewise PDF or REL.
      // Piecewise PDFs take care of themselves, but RELs need to be dealt with here.
      if( ( CTRel = chartType ) and ( not val ) ) then
        fraChartPointsEditor.isReadOnly := false
      else
        fraChartPointsEditor.isReadOnly := true
      ;

      fraFunctionParams.isReadOnly := isReadOnly;
    end
  ;


  procedure TFormChartPointsEditor2.setChartType( const val: TChartType );
    begin
      fraChartPointsEditor.chartType := val;
      fraFunctionParams.chartType := val;
    end
  ;

  
  function TFormChartPointsEditor2.getChartType(): TChartType;
    begin
      result := fraChartPointsEditor.chartType;
    end
  ;


  procedure TFormChartPointsEditor2.setAxisUnits( xAxisUnits, yAxisUnits: TChartUnitType );
    begin
      fraChartPointsEditor.XAxisUnits := xAxisUnits;
      fraChartPointsEditor.YAxisUnits := yAxisUnits;

      fraFunctionParams.xUnits := xAxisUnits;
      fraFunctionparams.yUnits := yAxisUnits;

      fraFunctionParams.unitsLocked := true;
      fraFunctionParams.cboXAxis.Enabled := false;
      fraFunctionParams.cboYAxis.Enabled := false;
    end
  ;

  function TFormChartPointsEditor2.getSaveChanges(): boolean; begin result := _saveChanges; end;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Functions for importing points
//-----------------------------------------------------------------------------
  procedure TFormChartPointsEditor2.importPoints( Sender: TObject );
    var
      errMsgSnippet: string;

      tmp: string;
      strToParse: string;
      pointsFile: TextFile;

      parser: TCSVContents;
      xColumn, yColumn: integer;
      i: integer;
      lastCompleteRow: integer;
      x, y: double;
      pointArray: T2DPointList;

      points: RPointArray;
      pdf: TPdfPiecewise;
      rel: TRelFunction;
    begin
      // Step 1: Get the string that will be parsed
      //--------------------------------------------
      dbcout( 'Step 1...', DBFORMCHARTPOINTSEDITOR );

      if( actionPastePoints = sender ) then // import/paste from clipboard
        begin
          errMsgSnippet := 'the clipboard';

          if Clipboard.HasFormat( CF_TEXT ) then
            strToParse := Clipboard.AsText
          else
            begin
              msgOK(
                tr( 'Clipboard data is not in text format: points could not be imported.' ),
                tr( 'Cannot import points' ),
                IMGCritical,
                self
              );
              exit;
            end
          ;
        end
      else if( actionImportPoints = sender ) then // import from file
        begin
          if( dlgImportCSV.Execute() ) then
            begin
              errMsgSnippet := abbrevPath( dlgImportCSV.FileName );
              try
                try
                  AssignFile( pointsFile, dlgImportCSV.FileName );
                  Reset( pointsFile );

                  while not eof( pointsFile ) do
                    begin
                      readLn( pointsFile, tmp );
                      strToParse := strToParse + tmp + endl;
                    end
                  ;
                except
                  msgOK(
                    ansiReplaceStr( tr( 'The file xyz' ), 'xyz', errMsgSnippet ) + ' ' + tr( 'could not be opened: it may be in use by another application.' ),
                    tr( 'Cannot import points' ),
                    IMGCritical,
                    self
                  );
                  exit;
                end;
              finally
                closeFile( pointsFile );
              end;
            end
          else
            exit
          ;
        end
      else
        begin
          raise exception.create( 'Wrong sender (' + (Sender as TComponent).Name + ') in TFormChartPointsEditor2.importPoints' );
          exit;
        end
      ;

      dbcout( 'Step 1 done.', DBFORMCHARTPOINTSEDITOR );


      // Step 2: Attempt to parse the CSV string
      //----------------------------------------
      dbcout( 'Step 2...', DBFORMCHARTPOINTSEDITOR );
      dbcout2( strToParse );
      
      parser := TCSVContents.createFromString( strToParse, true );

      if not( parser.parseSuccess ) then
        begin
          msgOK(
            ansiReplaceStr( tr( 'The contents of xyz could not be parsed.  Please check that you are using comma-delimited data.' ), 'xyz', errMsgSnippet ),
            tr( 'Parsing failed' ),
            IMGCritical,
            self
          );
          freeAndNil( parser );
          exit;
        end
      ;

      dbcout( 'Step 2 done', DBFORMCHARTPOINTSEDITOR );


      // Step 3: Check header row
      //-------------------------
      dbcout( 'Step 3...', DBFORMCHARTPOINTSEDITOR );

      xColumn := -1;
      yColumn := -1;

      // FIX ME: this search could be a bit more robust
      // (don't allow duplicate column names)
      for i := 0 to parser.columnCount - 1 do
        begin
          if( 'x' = fixup( parser.header(i) ) ) then xColumn := i;
          if( 'y' = fixup( parser.header(i) ) ) then yColumn := i;
        end
      ;

      // Check that required columns are present
      if( ( -1 = xColumn ) or ( -1 = yColumn ) ) then
        begin
          msgOK(
            ansiReplaceStr( tr( 'The contents of xyz are missing the "x" column and/or the "y" column.  Please check your data format.' ), 'xyz', errMsgSnippet ),
            tr( 'Parsing failed' ),
            IMGCritical,
            self
          );
          freeAndNil( parser );
          exit;
        end
      ;

      dbcout( 'Step 3 done', DBFORMCHARTPOINTSEDITOR );


      // Step 4: read the individual rows
      //---------------------------------
      dbcout( 'Step 4...', DBFORMCHARTPOINTSEDITOR );

      lastCompleteRow := 0;
      pointArray := T2DPointList.Create();

      try
        for i := 0 to parser.rowCount - 1 do
          begin
            Application.ProcessMessages();
            lastCompleteRow := i;

            //if( not( parser.rowIsEmpty( i ) ) ) then
              //begin
                x := myStrToFloat( parser.value( xColumn, i ), -1.0 );
                y := myStrToFloat( parser.value( yColumn, i ), -1.0 );

                // x values are allowed to be negative for a "generic" piecewise PDF.
                // Whether x values should be negative for distances and durations in NAADSM is a whole other matter...
                if( (* ( 0.0 > x ) or *) ( 0.0 > y ) ) then
                  begin
                    msgOK(
                      ansiReplaceStr( ansiReplaceStr( tr( 'The contents of xyz could not be parsed: a negative or nonnumeric value was found on line zxy.  Please check your data format.' ), 'xyz', errMsgSnippet ), 'zxy', intToStr( i ) ),
                      tr( 'Parsing failed' ),
                      IMGCritical,
                      self
                    );
                    freeAndNil( parser );
                    if( nil <> pointArray ) then
                      freeAndNil( pointArray )
                    ;
                    exit;
                  end
                else
                  pointArray.Append( T2DPoint.create( x, y ) )
                ;
              //end
            //;
          end
        ;
      except
        msgOK(
          ansiReplaceStr( ansiReplaceStr( tr( 'The contents of xyz could not be parsed: an error occurred at or near line zyx.  Please check your data format.' ), 'xyz', errMsgSnippet ), 'zyx', intToStr( lastCompleteRow ) ),
          tr( 'Parsing failed' ),
          IMGCritical,
          self
        );
        if( nil <> pointArray ) then
          freeAndNil( pointArray )
        ;
        freeAndNil( parser );
        exit;
      end;

      freeAndNil( parser );

      dbcout( 'Step 4 done', DBFORMCHARTPOINTSEDITOR );


      // Step 5: Check that the points are appropriate for the selected function type
      //------------------------------------------------------------------------------
      dbcout( 'Step 5...', DBFORMCHARTPOINTSEDITOR );

      pdf := nil;
      rel := nil;

      points := pointArray.createXYRecordArray();

      if( processPointsOK( points ) ) then
        begin
          // If we've made it this far, create the function object and set it
          if( CTPdf = chartType ) then
            begin
              pdf := TPdfPiecewise.create( points, fraFunctionParams.xUnits );
              pdf.debug();
              setFunction( pdf );
              freeAndNil ( pdf );
            end
          else if( CTRel = chartType ) then
            begin
              rel := TRelFunction.create( points, fraFunctionParams.xUnits, fraFunctionParams.yUnits );
              rel.debug();
              setFunction( rel );
              freeAndNil( rel );
            end
          else
            begin
              raise exception.Create( 'Chart type is unset in TFormChartPointsEditor2.importPoints' );
              setLength( points, 0 );
              if( nil <> pointArray ) then
                freeAndNil( pointArray )
              ;
              exit;
            end
          ;
        end
      ;

      dbcout( 'Step 5 done', DBFORMCHARTPOINTSEDITOR );


      // Cleanup
      //---------
      dbcout( 'Cleaning up...', DBFORMCHARTPOINTSEDITOR );
      setLength( points, 0 );
      if( nil <> pointArray ) then
        freeAndNil( pointArray )
      ;
      freeAndNil( pdf );
      freeAndNil( rel );
      dbcout( 'Done with cleanup', DBFORMCHARTPOINTSEDITOR );
    end
  ;


  function TFormChartPointsEditor2.ProcessPointsOK( var P: RPointArray ): boolean;
    var
      i: integer;
    begin
      Result := true;

      dbcout2( length( p ) );

      // There must be at least 3 points for a PDF, or 2 points for a relational function
      // FIX ME: I think we can get away with 1 point for a REL function now....
      if
        ( ( CTPdf = chartType ) and ( 3 > length( p ) ) )
      or
        ( ( CTRel = chartType ) and ( 2 > length( p ) ) )
      then
        begin
          msgOK(
            tr( 'There are too few points for this type of function.  Points cannot be imported.' ),
            tr( 'Cannot import points' ),
            IMGCritical,
            self
          );
          result := false;
          exit;
        end
      ;

      // X values must be sorted lowest to highest
      for i := low(p)+1 to high(p) do
        begin
          if( p[i].x < p[i-1].x ) then
            begin
              msgOK(
                tr( 'X values are not ordered from smallest to largest.  Points cannot be imported.' ),
                tr( 'Cannot import points' ),
                IMGCritical,
                self
              );
              result := false;
              exit;
            end
          ;
        end
      ;

      // For PDFs:
      // Check Ys histogram and pdf the same start and end with y=0
      // FIX ME: for now, cumulative pdfs cannot be imported.
      // Review Mark's old code (in ChartFunctions) should this ever be desirable.
      if( CTPdf = chartType) and not( (0= p[low(p)].y) and ( 0 = p[high(p)].y ) ) then
        begin
          msgOK(
            tr( 'First and last y values for a general/piecewise PDF must be 0.  Points cannot be imported.' ),
            tr( 'Cannot import points' ),
            IMGCritical,
            self
          );
          result := false;
          exit;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Functions for exporting points
//-----------------------------------------------------------------------------
  procedure TFormChartPointsEditor2.savePoints(Sender: TObject);
    var
      f: TextFile;
      fileSuccess: boolean;

      s: String;
      success: boolean;
      pointCount: integer;
    begin
      if( dlgExportCSV.Execute() ) then
        begin
          success := fraChartPointsEditor.pointsAsCSVToBuffer( s, pointCount );

          if( success ) then
            begin
              try
                try
                  AssignFile( f, dlgExportCSV.FileName );
                  Rewrite( f );
                  write( f, s );
                  fileSuccess := true;
                except
                  fileSuccess := false;
                end;
              finally
                closeFile( f );
              end;

              if( fileSuccess ) then
                msgOK(
                  ansiReplaceStr( ansiReplaceStr( tr( 'zyx points were written to file xyz.' ), 'zyx', intToStr( pointCount ) ), 'xyz', abbrevPath( dlgExportCSV.FileName ) ),
                  '',
                  IMGSuccess,
                  self
                )
              else
                msgOK(
                  ansiReplaceStr( tr( 'File xyz' ), 'xyz', abbrevpath( dlgExportCSV.FileName ) ) + ' ' + tr( 'could not be written.' ),
                  '',
                  IMGWarning,
                  self
                )
              ;
            end
          else
            msgOK(
              ansiReplaceStr( tr( 'Points could not be written to file xyz.' ), 'xyz', abbrevpath( dlgExportCSV.FileName ) ),
              '',
              IMGWarning,
              self
            )
          ;
        end
      ;
    end
  ;


 procedure TFormChartPointsEditor2.copyPoints(Sender: TObject);
  var
    s: String;
    success: boolean;
    pointCount: integer;
  begin
    success := fraChartPointsEditor.pointsAsCSVToBuffer( s, pointCount );

    if( success ) then
      begin
        ClipBoard.SetTextBuf( PChar(s) );
        msgOK(
          ansiReplaceStr( tr( 'zyx points were copied to the clipboard.' ), 'zyx', intToStr( pointCount ) ),
          '',
          IMGSuccess,
          self
        );
      end
    else
      msgOK(
        tr( 'Points could not be exported to the clipboard.' ),
        '',
        IMGWarning,
        self
      )
    ;
  end
;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Chart utilities and related functions
//-----------------------------------------------------------------------------
  procedure TFormChartPointsEditor2.saveChartToWMF( sender: TObject );
    var
      success: boolean;
    begin
      if( dlgSaveWMF.Execute ) then
        begin
          success := fraChartPointsEditor.saveChartToFile( dlgSaveWMF.FileName );

          if( success ) then
            begin
              if( sender = actionSaveChart ) then
                msgOK(
                  ansiReplaceStr( tr( 'This chart has been successfully saved to xyz.' ), 'xyz', abbrevPath( dlgSaveWMF.FileName ) ),
                  '',
                  IMGSuccess
                )
              ;
            end
          else
            msgOK(
              ansiReplaceStr( tr( 'This chart could not be saved to xyz.' ), 'xyz', abbrevPath( dlgSaveWMF.FileName ) ),
              '',
              IMGWarning
            )
          ;
        end
      ;

      fraChartPointsEditor.Repaint();
      repaint();
    end
  ;


  procedure TFormChartPointsEditor2.printChart( sender: TObject );
    var
      success: boolean;
    begin
      if( sender = actionPrintChart ) then
        begin
          if( not( dlgPrint.Execute() ) ) then exit;
        end
      ;

      success := fraChartPointsEditor.printChart();

      if( success ) then
        begin
          if( sender = actionPrintChart ) then
            msgOK(
              tr( 'The chart has been sent to the selected printer.' ),
              '',
              IMGSuccess
            )
          ;
        end
      else
        msgOK(
          tr( 'The chart could not be printed.' ),
          '',
          IMGWarning
        )
      ;

      fraChartPointsEditor.Repaint();
      repaint();
    end
  ;


  procedure TFormChartPointsEditor2.copyChartToClipboard( sender: TObject );
    var
      success: boolean;
    begin
      success := fraChartPointsEditor.copyChartToClipboard();

      if( success ) then
        begin
          if( sender = actionCopyChart ) then
            msgOK(
              tr( 'This chart has been successfully copied to the clipboard.' ),
              '',
              IMGSuccess
            )
          ;
        end
      else
        msgOK(
          tr( 'This chart could not be copied to the clipboard.' ),
          '',
          IMGWarning
        )
      ;

      fraChartPointsEditor.Repaint();
      repaint();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Other menu options
//-----------------------------------------------------------------------------
  procedure TFormChartPointsEditor2.ActionExitExecute(Sender: TObject);
    begin
      _saveChanges := false;
      Close();
    end
  ;


  procedure TFormChartPointsEditor2.ActionSaveExitExecute( sender: TObject );
    var
      fn: TChartFunction;
    begin
      try
        dbcout2( '** Starting TFormChartPointsEditor2.ActionSaveExitExecute...' );
        
        fn := createFunction();

        if( nil = fn ) then
          msgOK(
            tr( 'The function type is unspecified, or the parameters are not valid for the specified type.  Please correct this problem and try again.' ),
            tr( 'No function or invalid parameters' ),
            ImgInformation,
            self
          )
        else if( not( fn.isValid ) ) then
          msgOK(
            tr( 'Function parameters are not valid for the specified type.  Please correct this problem and try again.' ),
            tr( 'Invalid function parameters' ),
            IMGInformation,
            self
          )
        else if( 0 = length( fn.name ) ) then
          begin
            msgOK(
              tr( 'Please enter a name for this function.' ),
              tr( 'No name provided' ),
              IMGInformation,
              self
            );
            leTitle.SetFocus();
          end
        else
          begin
            _saveChanges := true;
            Close();
          end
        ;
      finally
        freeAndNil( fn );
      end;

      dbcout2( '** Done with TFormChartPointsEditor2.ActionSaveExitExecute' );
    end
  ;


  procedure TFormChartPointsEditor2.ActionHelpExecute(Sender: TObject);
    begin

      if( isReadOnly ) then
        msgOK(
          tr( 'This is a read-only chart: it may be viewed, exported or printed, but not modified.' ),
          tr( 'Chart editor' ),
          IMGInformation,
          self
        )
      else
        begin
          if
            ( ( CTPdf = chartType ) and ( 4 = fraFunctionParams.cboPdfType.ItemIndex ) )
          or
            ( CTRel = chartType )
          then
            msgOK(
              tr( 'Drag points with the left mouse button to reposition them.' ) + ' ' + endl + endl
                + tr( 'Right click on points to remove or edit the position of a point.' ) + ' ' + endl + endl
                + tr( 'Right click on a position to create a new point.' ) + ' ' + endl + endl
                + tr( 'Drag points off the chart to expand the axes.' ) + endl + endl
                + tr( 'Alternatively, edit the point values in the grid shown on the left side of the screen.' )
                + '  ' + tr( 'Click ''OK'' to apply your changes.' ),
              tr( 'Chart editor' ),
              IMGInformation,
              self
            )
          else
            msgOK(
              tr( 'Select a type of probability density function from the list, and enter all of the required parameters.' )
                + '  ' + tr( 'Click on the green checkmark to apply the changes and view the function.' ) + endl + endl
                + tr( 'If no chart is displayed, correct the problems described in the error message shown on the right side of the window.' ),
              tr( 'Chart editor' ),
              IMGInformation,
              self
            )
          ;
        end
      ;


    end
  ;
//-----------------------------------------------------------------------------




//*****************************************************************************
// Unrevised code
//*****************************************************************************


//-----------------------------------------------------------------------------
// AR 7/14/05
// Right now, cumulative PDFs cannot be imported.
// Keep this code around in case we want to some day.
//-----------------------------------------------------------------------------
(*
interface

  function PatternOfCumulativeOK(  var P: RPointArray ): boolean;
  procedure AddPoint( var P: RPointArray; X, Y: double );
  procedure AddIntervalBar(  var P: RPointArray; P1X, P2X, Slope: double );
  function GetSlope(P1, P2: RPoint; var Ok: boolean): double;
  function ConvertCumulativeToDensityOK( var P: RPointArray ): boolean;



implementation


  function PatternOfCumulativeOK(  var P: RPointArray ): boolean;
    var
      lastY : double;
      i  : longint;
      PositiveSlope : boolean;
    begin
      PositiveSlope := True;
      lastY := P[0].Y;

      for i := Low(P) + 1 to High(P) do
        begin
          if P[i].Y < LastY then
            begin
              PositiveSlope := False;
              Break;
            end
          else
            LastY := P[i].Y
          ;
        end
      ;

      if not PositiveSlope then
        ShowMessage('These points do not fit the pattern of a'
          + ' cumulative density function because the Y values decrease '
          + 'between points ' + IntToStr(i) + ' and ' + IntToStr(i+1) + '.')
      ;

      Result := PositiveSlope;
    end
  ;


  // Subhelper functions in convertCumulativeToDensityOK
  procedure AddPoint( var P: RPointArray; X, Y: double );
    begin
      SetLength(P, Length(P) + 1);
      P[High(P)].X := X;
      P[High(P)].Y := Y;
    end
  ;


  procedure AddIntervalBar(  var P: RPointArray; P1X, P2X, Slope: double );
    begin
      AddPoint(P, P1X, Slope);
      AddPoint(P, P2X, Slope);
    end
  ;


  function GetSlope(P1, P2: RPoint; var Ok: boolean): double;
    var
      n, d : double;
    begin
      n := (P2.Y - P1.Y);
      d := (P2.X - P1.X);
      OK := (d > 0); // discontinuous

      if OK then
        Result := n/d
      else
        Result := 0
       ;
    end
  ;


  function ConvertCumulativeToDensityOK( var P: RPointArray ): boolean;
  var
    i : longint;
    CP : RPointArray;
    xValLow, xValHigh, Slope : double;
    OK : boolean;
  begin 
    OK := True;
    SetLength(CP, Length(P));

    try
      // Copy array to OP
      for i := Low(P) to High(P) do Move(P[i], CP[i], Sizeof(CP[i]));
    
      // clear P
      P := nil;
    
      // initialize
      xValLow := CP[0].X;
      xValHigh := CP[High(CP)].X;
      AddPoint(P, xValLow, 0);

      // differentiate lines and add bars
      for i := Low(CP) to High(CP) - 1 do
        begin
          Slope := GetSlope(CP[i],CP[i+1], Ok);

          if not OK then
            begin
              P := nil;
              Break;
            end
          ;

          AddIntervalBar(P, CP[i].X, CP[i+1].X, Slope);
        end
      ;

      // end point
      if OK then
        AddPoint(P, xValHigh, 0)
      else
        ShowMessage('This cumulative density function is ' +
          'discontinuous and thus cannot be converted ' +
          'to a PDF')
      ;
    finally
      CP := nil;
    end;

    Result := OK;
  end
  ;
*)
//-----------------------------------------------------------------------------



end.
