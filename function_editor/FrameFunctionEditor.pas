unit FrameFunctionEditor;

(*
FrameFunctionEditor.pas/dfm
---------------------------
Begin: 2006/01/04
Last revision: $Date: 2008/11/25 22:05:57 $ $Author: areeves $
Version number: $Revision: 1.1 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2008 Animal Population Health Institute, Colorado State University

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

    Models,
    ChartFunction,
    FunctionDictionary,
    FunctionEnums
  ;

  {*
    This widget is used to display, create, and edit probability density and relational functions.  PDFs and
    REL functions are all derived from TChartFunction.  (See the units ChartFunction, ProbDensityFunctions, and
    RelFunction for more information.)  The term "chart" is occasionally used to refer to a PDF or REL function,
    because nearly all functions are displayed and/or edited in graphical form as a chart.

    An item that has one or more PDFs or RELs is referred to as a "model": for example, a model of a pair of dice
    would have two uniform PDFs to represent the possible outcomes of the roll of each die.  Any model that uses
    a PDF or a REL function must inherit from TModelWithFunctions: see the unit Models for more information.

    All of the functions are contained in a master list (see unit FunctionDictionary) that is owned at the top-most
    data structure for an entire simulation.  This top-most data structure inherits from TSimInput (see unit
    SimInput).  A function in the master list can be used by zero, one, or multiple models, which are also owned by the
    top-most data structure.

    The function for only one model is shown at a time (see _model): it is the responsibility of the containing
    form or frame to indicate which model is the current one (i.e., _model).  When a function is edited or removed,
    this widget is responsible for ensuring that every model that uses the function is updated.  For this reason,
    this widget may require access to a list of all models: see TModelList and _modelList.  If _modelList is nil, then
    this widget assumes that the current model is the only one that needs to be updated.


    // FIX ME: The rest of this comment needs to be revised.
    This widget gives the user a list of available functions from which one may be selected.  Use appendFunction()
    to add functions to this list.

    Once several functions are available, use setChart() to indicate the currently selected function.

    The user may alter (edit) the selected function, or create a brand new one.  The user may clear the current
    selection, or remove the current selection from the list entirely.

    Instances of TFrameFunctionEditor work with instances of FormChartPointsEditor2.  It is the form
    that provides the graphical interface for manipulating the functions themselves.

    // FIX ME: This paragraph is no longer correct.
    This control handles all function creation and editing, but it does not manage application-specific
    data structures.  If your application uses any specialized data structures, do not use this class directly.
    Instead, derive a new class from this base class, and reimplement the constructor and the functions
    setChart(), editDone(), clearChart() and removeChart() to work with your specific data structures.  See
    unit FrameSMFunctionEditor for examples of how these functions are reimplemented to accomodate the
    data structures used in NAADSM.

    This widget operates on one item (or "model") in a list of models.

    Key properties:
    ---------------

    chartType: indicates whether the editor works with PDFs or REL functions.  One or the other must be specified:
      a single editor cannot handle both types.

    maxY, minY: potentially used with REL functions, if there is a maximum or minimum allowable value along the Y axis.
     (These properies are not actually used in NAADSM, but seemed like a good idea at the time.)

    xUnits, yUnits: specify the units that will be displayed along the two axes.  PDfs have units only on the
      x axis: y values are set to give an area under the curve of 1, but really don't have units.
      REL functions should have units along both axes.

    unitsLocked: if units are locked, the user may not change them.  In nearly every situation that I can
      currently imagine, units should be locked.
  }

  type TAppUpdatePtr = procedure() of object;

  type TFrameFunctionEditor = class( TFrame )
      cboChartList: TComboBox;
      lblFunctionDescr: TLabel;

      btnNew: TButton;
      btnRemove: TButton;
      btnEdit: TButton;
      btnClear: TButton;
      lblDisabled: TEdit;

      procedure cboChartListChange(Sender: TObject);

      procedure btnNewClick(Sender: TObject);
      procedure btnRemoveClick(Sender: TObject);
      procedure btnEditClick(Sender: TObject);
      procedure btnClearClick(Sender: TObject);

    protected
      // For internal use
      _editing: boolean;
      _creating: boolean;

      _myForm: TForm;

      // Properties of this object
      _enabled: boolean;
      _chartType: TChartType;

      // Properties passed through to the chart points editor when necessary
      _unitsLocked: boolean;
      _nameLocked: boolean;
      _nameAlwaysVisible: boolean;
      _xUnits: TChartUnitType;
      _yUnits: TChartUnitType;
      _minY: double;
      _maxY: double;

      { The selected chart.  Nil if no chart is selected. }
    	_chartFn: TChartFunction;

      { The thing (e.g. production type or prod type pair) that has a chart as a parameter. }
      _model: TModelWithFunctions;

      { The list of things, the members of which might have the chart as a parameter. }
      _modelList: TModelList;

      { The application-specific data structure that keeps track of all functions in the simulation. }
      _functionDict: TFunctionDictionary;

      { Identifies the specific chart field (e.g. a disease period) that this editor applies to. }
      _whichChart: TSMChart;

      { A value used by some models to convey extra information about which chart is being edited. }
      _addlInfo: integer;

      { These function pointers can be used to update the appearance of the form or frame that
        contains this function editor.  Alternatively, inherit from this class and override the
        following functions: updateAppDisplayForChartChange(), updateAppDisplay() }
      _ptrUpdateAppDisplayForChartChange: TAppUpdatePtr;
      _ptrUpdateAppDisplay: TAppUpdatePtr;

      procedure translateUI();

      function multipleEffectsOK( const idx: integer ): boolean; virtual;
      function removalEffectsOK( const idx: integer ): boolean; virtual;

      procedure updateAppDisplayForChartChange( fn: TChartFunction ); virtual;
      procedure updateAppDisplay(); virtual;

      procedure setAdditionalInfo( val: integer );

      { Creates an empty chart points editor to allow the user to create a new chart/function. }
      procedure newChart();

      { Creates a chart points editor and fills it with the selected function to allow editing. }
      procedure editChart( const idx: integer );

      { Called when chart creation or editing is complete. Reimplement to allow the application to do something with the resulting chart. }
      procedure editDone( newFn: TChartFunction );

      { Used with cboChartListChange() to select one of the available functions.  Maybe this should be public? }
      procedure setChart( const idx: integer );

      { Removes the selected function from the list.  Reimplement to remove function from application's data structures. }
      procedure removeChart( const idx: integer );

      { Clears the current selection, but does not remove the selected function from the list. }
      procedure clearChart();

      // Properties of this object
      procedure setEnabled( val: boolean ); reintroduce;
      function getEnabled(): boolean; reintroduce;

      procedure setChartType( val: TChartType );
      function getChartType(): TChartType;

      // Properties passed through to the chart points editor when necessary
      procedure setUnitsLocked( val: boolean );
      procedure setXUnits( val: TChartUnitType );
      procedure setYUnits( val: TChartUnitType );
      procedure setMinY( val: double );
      procedure setMaxY( val: double );

      function getUnitsLocked(): boolean;
      function getXUnits(): TChartUnitType;
      function getYUnits(): TChartUnitType;
      function getMinY(): double;
      function getMaxY(): double;

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      { Used to specify the containing form, mostly so that dialogs are properly centered. }
      procedure setForm( frm: TForm );

      { For adding functions to to cboChartList. }
      procedure appendFunction( fn: TChartFunction );

      { Removes all functions from the chart list. }
      procedure clearList();

      { Property-like functions }
      procedure setModelList(  list: TModelList );
      procedure setFunctionDict( list: TFunctionDictionary );
      procedure setChartField( ch: TSMChart );

      { Shortcut for setting _model, _chartFn, and _whichChart all at once. }
      procedure showChart( p: TModelWithFunctions; c: TChartFunction; ch: TSMChart );

      property additionalInfo: integer write setAdditionalInfo;

      // Properties of this object
      property enabled: boolean read getEnabled write setEnabled;
      property chartType: TChartType read getChartType write setChartType;

      property ptrUpdateAppDisplayForChartChange: TAppUpdatePtr write _ptrUpdateAppDisplayForChartChange;
      property ptrUpdateAppDisplay: TAppUpdatePtr write _ptrUpdateAppDisplay;

      // Properties passed through to the chart points editor when necessary
      property unitsLocked: boolean read getUnitsLocked write setUnitsLocked;
      property xUnits: TChartUnitType read getXUnits write setXUnits;
      property yUnits: TChartUnitType read getYUnits write setYUnits;
      property minY: double read getMinY write setMinY;
      property maxY: double read getMaxY write setMaxY;

    end
  ;


implementation

{$R *.dfm}

  uses
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    ControlUtils,
    MyDialogs,
    I88n,

    ProbDensityFunctions,
    RelFunction,

    FormChartPointsEditor2
  ;

  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit

//-----------------------------------------------------------------------------
// Construction/destruction
//-----------------------------------------------------------------------------
  constructor TFrameFunctionEditor.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      _modelList := nil;

      _ptrUpdateAppDisplayForChartChange := nil;
      _ptrUpdateAppDisplay := nil;

      _addlInfo := -1;
      _myForm := nil;
      _unitsLocked := true;
      _nameAlwaysVisible := false;

      lblFunctionDescr.Caption := tr( '(No function is selected)' );

      lblDisabled.Top := cboChartList.Top;
      lblDisabled.Left := cboChartList.Left;
      lblDisabled.Height := cboChartList.Height;
      lblDisabled.Width := cboChartList.Width;

      cboChartList.Items.Clear();
      cboChartListChange( nil );

    	setEnabled( true );
    end
  ;


  procedure TFrameFunctionEditor.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:50:37 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/function_editor/FrameFunctionEditor.dfm
      // File date: Mon Feb 25 15:25:29 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblFunctionDescr.Caption := tr( 'lblFunctionDescr' );
          btnNew.Caption := tr( 'New...' );
          btnNew.Hint := tr( 'Click to create a new function' );
          btnRemove.Caption := tr( 'Remove' );
          btnRemove.Hint := tr( 'Click to permanently remove the specified function' );
          btnEdit.Caption := tr( 'Edit...' );
          btnEdit.Hint := tr( 'Click to alter the specified function' );
          btnClear.Caption := tr( 'Clear' );
          btnClear.Hint := tr( 'Click to clear the selected function for this parameter' );
          lblDisabled.Text := tr( '(Unused/disabled)' );
        end
      ;

    end
  ;


  destructor TFrameFunctionEditor.destroy();
    begin
      // Nothing to do in here yet
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Unlabeled stuff
//-----------------------------------------------------------------------------
  {*
    Called when chart creation or editing is complete.
    Reimplement to allow the application to do something with the resulting chart.

    @param newFn The function that was just created or altered.
  }
  procedure TFrameFunctionEditor.editDone( newFn: TChartFunction );
  	var
    	cht: TChartFunction;
      newItem, tmpItem: TFunctionDictionaryItem;
      wasNew:  boolean;

      oldChartName: string;
      newChartName: string;

      { Used to obtain a unique name for the newly created/edited chart. }
      function getUniqueNameOK( var chartName: string ): boolean;
        var
          dlgValue: integer;
        begin
          dlgValue := msgInputOKCancel(
            tr( 'This function name is already in use.  Please enter a unique name.' ),
            chartName,
            '',
            tr( 'Duplicate function name' ),
            IMGQuestion,
            _myForm
          );

          if( mrCancel = dlgValue ) then
            result := false
          else if( _functionDict.contains( chartName ) ) then
            result := getUniqueNameOK( chartName )
          else
            result := true
          ;
        end
      ;

  	begin
    	dbcout( 'CHART EDITING IS DONE', DBSHOWMSG );
      // _chartFn is the existing function
      // cht is the new function, created below

      if( CTPdf = _chartType ) then
        cht := createPdfFromBaseObject( newFn as TPdf )
      else if( CTRel = _chartType ) then
        cht := createRelFromBaseObject( newFn as TRelFunction )
      else
        raise exception.Create( 'Unrecognized chart type in TFrameSMFunctionEditor.editDone' )
      ;

      cht.dbField := ord( _whichChart );

      if( _creating ) then // A new chart was just created
      	begin
          //  Check for duplicate chart name
          //--------------------------------
          newChartName := cht.name;

          if( _functionDict.contains( cht.name ) ) then
            begin
              // The newly entered function name is a duplicate and is not allowed.
              // Get a legitimate new name, and move on.
              if( getUniqueNameOK( newChartName ) ) then
                begin
                  dbcout( 'New name is OK!', DBSHOWMSG );
                  cht.name := newChartName;
                end
              else // The *&$%#& user cancelled the name dialog box.
                begin
                  dbcout( 'User cancelled', DBSHOWMSG );
                  exit;
                end
              ;
            end
          ;

          // Add the new chart to the master function list
          //-----------------------------------------------
          tmpItem := TFunctionDictionaryItem.create( cht );
          tmpItem.new := true;
          tmpItem.incrRefCounter();
          _functionDict.insert( cht.name, tmpItem );

          // Change the selected chart field in the current thing
          //-----------------------------------------------------
          if( nil <> _model.chart( _whichChart, _addlInfo ) ) then
          	begin
            	_model.removeChart( cboChartList.items.strings[cboChartList.itemIndex] );
              if( nil <> _functionDict.value( cboChartList.items.strings[cboChartList.itemIndex] ) ) then
          		  _functionDict.value( cboChartList.items.strings[cboChartList.itemIndex] ).decrRefCounter()
              ;
            end
          ;
          _model.setChart( _whichChart, cht, _addlInfo );
        end
      else if( _editing ) then // An existing chart was changed
      	begin
          // Check the chart names
          //----------------------
          oldChartName := _chartFn.name;
          newChartName := cht.name;

          if( oldChartName = newChartName ) then
            // Everything is fine.  Just move along.
          else
            begin
              if( not( _functionDict.contains( newChartName ) ) ) then
                begin
                  // The new name is OK.
                  // Change the name in the dictionary and move on.
                  _functionDict.Rename( oldChartName, newChartName );
                end
              else
                begin
                  // The newly edited function name is a duplicate and is not allowed.
                  // Get a legitimate new name, and move on.
                  if( getUniqueNameOK( newChartName ) ) then
                    begin
                      cht.name := newChartName;
                      _functionDict.Rename( oldChartName, newChartName );
                    end
                  else // The *&$%#& user cancelled the name dialog box.
                    exit
                  ;
                end
              ;
            end
          ;


          // Replace the old version of the chart
          // with the new one in the master function list
          // and in the current model.
          //----------------------------------------------
        	cht.id := _functionDict.value( newChartName ).fn.id;
          newItem := TFunctionDictionaryItem.create( cht );
          newItem.refCounter := _functionDict.value( newChartName ).refCounter;

          dbcout( 'Chart is used ' + intToStr( newItem.refCounter ) + ' times', DBSHOWMSG );

          dbcout( 'Replacing existing chart ' + newChartName, DBSHOWMSG );
          // Was the existing chart newly created? (see below)
          wasNew := _functionDict.value( cht.name ).new;

          dbcout( 'Freeing old chart ' + newChartName, DBSHOWMSG );
          _functionDict.delete( cht.name );

          dbcout( 'Creating new chart ' + newChartName, DBSHOWMSG );
          _functionDict.insert( cht.name, newItem );

          // If the changed chart was new, it is still new.
          // Otherwise, set the modified flag.
          if( wasNew ) then
          	begin
          		_functionDict.value( newChartName ).new := true;
              _functionDict.value( newChartName ).modified := false;
            end
          else
          	begin
            	_functionDict.value( newChartName ).new := false;
          		_functionDict.value( newChartName ).modified := true;
            end
        	;

          if( nil <> _modelList ) then
            _modelList.changeChart( _whichChart, oldChartName, cht, _addlInfo )
          else
            //raise exception.create( '_modelList is nil in TFrameSMFunctionEditor.editDone' )
            // The line above is obsolete, now FrameFunctionEditor can operate on only a single model.
            _model.changeChart( _whichChart, oldChartName, cht, _addlInfo )
          ;
        end
      ;

      dbcout( '--- Updating master display...', DBSHOWMSG );
      updateAppDisplay();

      (*
      // THE PREVIOUS VERSION OF THIS FUNCTION:
      showMessage( 'Editing is done.  Override this function to do something useful.' );

      if( nil <> newFn ) then
        begin
          lblFunctionDescr.Caption := newFn.descr;
          newFn.debug();
        end
      else
        lblFunctionDescr.Caption := tr( '(No function is selected)' )
      ;

      _editing := false;
      _creating := false;
      *)
    end
  ;


  function TFrameFunctionEditor.removalEffectsOK( const idx: integer ): boolean;
    var
      listItem: TFunctionDictionaryItem;
      response: integer;
    begin
      result := true;

      listItem := _functionDict.value( cboChartList.items.strings[idx] );

      // Make sure that the user knows that the function will be blown away.
      //--------------------------------------------------------------------
      response := msgYesNo(
        tr( 'Removing this function will permanently delete it from the scenario.' )
          + '  ' + tr( 'If you want to keep this function in the scenario file but not apply it here, select ''Clear'' instead.' )
          + endl + endl
          + tr( 'Continue?' ),
        tr( 'Function will be permanently removed' ),
        IMGQuestion,
        _myForm
      );

      if( response = mrNo ) then result := false;

      if( true = result ) then
        begin
          // Make sure that the user knows that there will be multiple effects
          //------------------------------------------------------------------
          if( listItem.refCounter > 1 ) then
            begin
              response := msgYesNo(
                tr( 'This function is used in several places.  Removing it will affect all of them.  Continue?' ),
                tr( 'Multiple function instances' ),
                IMGQuestion,
                _myForm
              );

              if( response = mrNo ) then result := false;
            end
          ;
        end
      ;
    end
  ;


  procedure TFrameFunctionEditor.updateAppDisplay();
    begin
      if( nil <> @_ptrUpdateAppDisplay ) then
        _ptrUpdateAppDisplay()
      ;
    end
  ;


  {*
    Removes the selected function from the list.  Reimplement to
    remove function from application's data structures.

    @param idx Index of the chart (in cboChartList) to remove
  }
  procedure TFrameFunctionEditor.removeChart( const idx: integer );
  	begin
      if( removalEffectsOK( idx ) ) then
        begin
          if( nil <> _modelList ) then
            _modelList.removeChart( cboChartList.items.strings[idx] )
          else
            _model.removeChart( cboChartList.items.strings[idx] )
          ;
          _functionDict.value( cboChartList.items.strings[idx] ).removed := true;

          _model.setChart( _whichChart, nil, _addlInfo );

          updateAppDisplay();
        end
      ;
    end
  ;

  
  {*
    Clears the current selection, but does not remove the selected function from the list.
  }
  procedure TFrameFunctionEditor.clearChart();
    begin
      _model.setChart( _whichChart, nil, _addlInfo );
      
      updateAppDisplay();
    end
  ;


  procedure TFrameFunctionEditor.updateAppDisplayForChartChange( fn: TChartFunction );
    begin
      if( nil <> @_ptrUpdateAppDisplayForChartChange ) then
        _ptrUpdateAppDisplayForChartChange()
      ;
    end
  ;


  {*
    Used with cboChartListChange() to select one of the available functions.
    FIX ME: consider making this a public function.

    @param idx Index of the chart (in cboChartList) to set
  }
  procedure TFrameFunctionEditor.setChart( const idx: integer );
  	var
    	fn: TChartFunction;
      oldCount: Integer;
  	begin
      if( 0 <= idx ) then
        begin
          dbcout( endl, DBSHOWMSG );

          // Decrement the reference counter for the old list item...
          if( nil <> _chartFn ) then
            begin
              dbcout2( 'Old Chart Function ' + _chartFn.name + ' had a ref count of: ' + IntToStr(_functionDict.value(_chartFn.name).refCounter), DBSHOWMSG );
              _functionDict.value(_chartFn.name).decrRefCounter();
              dbcout2( 'Old Chart Function ' + _chartFn.name + ' now has a ref count of: ' + IntToStr(_functionDict.value(_chartFn.name).refCounter), DBSHOWMSG );
            end
          else
            dbcout2( 'Old Chart Function was nil', DBSHOWMSG )
          ;

          // ...set the new list item...
          fn := cboChartList.items.objects[idx] as TChartFunction;
          _chartFn := _functionDict.value(fn.name).fn;

          // ...update the application to show that a change was made...
          updateAppDisplayForChartChange( fn );

          dbcout( endl, DBSHOWMSG );
          
          // ... and increment the counter for the new list item.
          dbcout( 'New Chart Function ' + _chartFn.name + ' had a ref count of: ' + IntToStr(_functionDict.value(_chartFn.name).refCounter), DBSHOWMSG );
          _functionDict.value(_chartFn.name).incrRefCounter();
          dbcout( 'New Chart Function ' + _chartFn.name + ' now has a ref count of: ' + IntToStr(_functionDict.value(_chartFn.name).refCounter), DBSHOWMSG );

          // Finally, remember to update the production type.
          oldCount := _functionDict.value(_chartFn.name).refCounter;
          _model.setChart( _whichChart, _chartFn, _addlInfo );
          
          //SetChart Functions incr the ref counter......set it back.
          //  This fixes the bug where charts, which should only have a ref count of 1
          //  have higher counts....the setChart() function in their respective units, such as ProductionType, etc...
          //  incremented the refcount.  Check it here to be sure it wasn't changed and if it was, set it back.
          if  ( _functionDict.value(_chartFn.name).refCounter <> oldCount ) then
            _functionDict.value(_chartFn.name).refCounter := oldCount
          ;
            
        end
      else
        // This happens once in a while during initial setup.  Just ignore it.
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Function creation and editing
//-----------------------------------------------------------------------------
  {*
    Creates an empty chart points editor to allow the user to create a new chart/function.

    If the user accepts the newly created chart, editDone() is called.
  }
  procedure TFrameFunctionEditor.newChart();
    var
      frm: TFormChartPointsEditor2;
      newFn: TChartFunction;
    begin
      frm := nil;
    	_editing := false;
      _creating := true;

      try
        case _chartType of
          CTPdf, CTRel:
            begin
              frm := TFormChartPointsEditor2.create(
                _myForm, // AOwner: TComponent;
                chartType, // const chartType: TChartType;
                xUnits, // const xAxisUnits: TChartUnitType = UnitsUnknown;
                yUnits, // const yAxisUnits: TChartUnitType = UnitsUnknown;
                true, // const unitsLocked: boolean = true;
                minY, // const minY: double = 0.0;
                maxY, // const maxY: double = 0.0;
                '', // const title: string = '';
                false, // const titleLocked: boolean = true;
                false // const readOnly: boolean = false
              );
            end
          ;
          else
            raise exception.create( 'Unrecognized chartType in TFrameFunctionEditor.newChart' )
          ;
        end;

        frm.ShowModal();

        if( frm.changesSaved ) then
          begin
            newFn := frm.createFunction();
            editDone( newFn );
            freeAndNil( newFn );
          end
        else
          dbcout( 'Changes to new chart were not saved', DBSHOWMSG )
        ;

      finally
        _editing := false;
        _creating := false;
        freeAndNil( frm );
      end;
    end
  ;


  function TFrameFunctionEditor.multipleEffectsOK( const idx: integer ): boolean;
    var
    	listItem: TFunctionDictionaryItem;
      response: integer;
    begin
      result := true;

    	listItem := _functionDict.value( cboChartList.items.strings[idx] );

      // Make sure that the user knows that there will be multiple effects
      //------------------------------------------------------------------
      if( listItem.refCounter > 1 ) then
      	begin
          response := msgYesNo(
            tr( 'This function is used in several places.  Altering it will affect all of them.  Continue?' ),
            tr( 'Do not show this message again' ),
            IMGQuestion,
            _myForm
          );

          if( response = mrNo ) then result := false;
        end
      ;
    end
  ;


  {*
    Creates a chart points editor and fills it with the selected function to allow editing.

    If the user accepts the altered chart, editDone() is called.
  }
  procedure TFrameFunctionEditor.editChart( const idx: integer );
    var
      frm: TFormChartPointsEditor2;
      oldChart: TChartFunction;
      newChart: TChartFunction;
    begin
      _editing := true;
      _creating := false;

      try
        if( not multipleEffectsOK( idx ) ) then
          exit
        ;

        dbcout( '--- Start TFrameFunctionEditor.editChart', DBSHOWMSG );
        oldChart := cboChartList.Items.Objects[ cboChartList.ItemIndex ] as TChartFunction;

        _chartType := oldChart.chartType;

        if( oldChart is TPdfPiecewise ) then
          begin
            if( 3 > length( (oldChart as TPdfPiecewise).pointArray ) ) then
              begin
                msgOK(
                  tr( 'There are not enough points to specify this function.' )
                    + '  ' + tr( 'A default set of points will be created.' ),
                  tr( 'Too few points' ),
                  IMGInformation,
                  _myForm
                );
              end
            ;
          end
        ;

        if( oldChart is TRelFunction ) then
          begin
            if( 1 > length( (oldChart as TRelFunction).pointArray ) ) then
              begin
                msgOK(
                  tr( 'There are not enough points to specify this function.' )
                    + '  ' + tr( 'A default set of points will be created.' ),
                  tr( 'Too few points' ),
                  IMGInformation,
                  _myForm
                );
              end
            ;
          end
        ;

        case _chartType of
          CTPdf, CTRel:
            begin
              frm := TFormChartPointsEditor2.create(
                _myForm, // AOwner: TComponent;
                oldChart, // const chart: TChartFunction;
                minY, // const minY: double = 0.0;
                maxY, // const maxY: double = 0.0;
                false // const readOnly: boolean = false;
              );
            end
          ;
          else
            raise exception.create( 'Unrecognized chartType in TFrameFunctionEditor.editChart' )
          ;
        end;

        frm.ShowModal();

        if( frm.changesSaved ) then
          begin
            newChart := frm.createFunction();
            self.editDone( newChart );
            freeAndNil( newChart );
          end
        else
          dbcout( 'Changes to existing chart were not saved', DBSHOWMSG )
        ;

         dbcout( '--- done TFrameFunctionEditor.editChart', DBSHOWMSG );
      finally
        _editing := false;
        _creating := false;
        freeAndNil( frm );
      end;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Function list handling
//-----------------------------------------------------------------------------
  {*
    Removes all functions from the chart list.
  }
  procedure TFrameFunctionEditor.clearList();
  	begin
      cboChartList.ItemIndex := -1;
      lblFunctionDescr.Caption := tr( '(No function is selected)' );
      cboChartList.Clear();
    end
  ;


  {*
    For adding functions to to cboChartList.

    @param fn The instance of TChartFunction (PDF or REL) to add to the list
  }
  procedure TFrameFunctionEditor.appendFunction( fn: TChartFunction );
    begin
      if( CTUnspecified = chartType ) then chartType := fn.chartType;

      if( chartType <> fn.chartType ) then
        raise exception.create( 'Wrong kind of chart is being added in TFrameFunctionEditor.appendFunction' )
      else
        cboChartList.AddItem( fn.name, fn )
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handling
//-----------------------------------------------------------------------------
  procedure TFrameFunctionEditor.btnNewClick(Sender: TObject);
  	begin
    	newChart();
    end
  ;

  procedure TFrameFunctionEditor.btnRemoveClick(Sender: TObject);
    begin
      if cboChartList.ItemIndex > -1 then removeChart( cboChartList.ItemIndex );
    end
  ;


  procedure TFrameFunctionEditor.btnClearClick(Sender: TObject);
    begin
      if cboChartList.ItemIndex > -1 then clearChart();
    end
  ;


  procedure TFrameFunctionEditor.btnEditClick(Sender: TObject);
    begin
      if( -1 = cboChartList.ItemIndex ) then
        newChart()
      else
        editChart( cboChartList.ItemIndex )
      ;
    end
  ;


  procedure TFrameFunctionEditor.cboChartListChange(Sender: TObject);
    begin
      if( -1 = cboChartList.ItemIndex ) then
        begin
          lblFunctionDescr.Caption := tr( '(No function is selected)' );
          btnEdit.Enabled := false;
          btnRemove.Enabled := false;
          btnClear.Enabled := false;
        end
      else
        begin
          lblFunctionDescr.Caption := ( cboChartList.Items.Objects[ cboChartList.itemIndex ] as TChartFunction ).descr;
          btnEdit.Enabled := true;
          btnRemove.Enabled := true;
          btnClear.Enabled := true;
        end
      ;

      setChart( cboChartList.ItemIndex );
    end
  ;

  procedure TFrameFunctionEditor.showChart( p: TModelWithFunctions; c: TChartFunction; ch: TSMChart );
    var
      i: integer;
  	begin
   		_chartFn := c;
      _whichChart := ch;
      _model := p;

      if( nil <> c ) then
        begin
          i := cboChartList.Items.IndexOf( c.name );
          if( i > -1 ) then
            cboChartList.ItemIndex := i
          else
            cboChartList.ItemIndex := -1;
          ;
        end
      else
        cboChartList.itemIndex := -1
      ;

      cboChartListChange( nil );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Initial setup (property-like functions)
//-----------------------------------------------------------------------------
  procedure TFrameFunctionEditor.setFunctionDict( list: TFunctionDictionary );
  	begin
   		_functionDict := list;
    end
  ;


  procedure TFrameFunctionEditor.setModelList( list: TModelList );
  	begin
    	_modelList := list;
    end
  ;


  procedure TFrameFunctionEditor.setChartField( ch: TSMChart );
    begin
      _whichChart := ch;
    end
  ;


  procedure TFrameFunctionEditor.setAdditionalInfo( val: integer );
    begin
      _addlInfo := val;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties and related functions
//-----------------------------------------------------------------------------
  {*
    Used to specify the containing form, mostly so that dialogs are properly centered.

    @param frm Reference to a form (usually but not necessarily the container)
  }
	procedure TFrameFunctionEditor.setForm( frm: TForm );
  	begin
   		_myForm := frm;
    end
  ;

  // Properties for this object
	procedure TFrameFunctionEditor.setEnabled( val: boolean );
  	begin
      if( -1 = cboChartList.ItemIndex ) then
        begin
          btnEdit.Enabled := false;
          btnRemove.Enabled := false;
          btnClear.Enabled := false;
        end
      else
        begin
          btnEdit.Enabled := val;
          btnRemove.Enabled := val;
          btnClear.Enabled := val;
        end
      ;

      btnNew.Enabled := val;
      lblFunctionDescr.Visible := val;
      cboChartList.Enabled := val;
      cboChartList.Visible := val;

      lblDisabled.Visible := not( val );

      _enabled := val;
    end
  ;

  function TFrameFunctionEditor.getEnabled(): boolean; begin result := _enabled; end;

  procedure TFrameFunctionEditor.setChartType( val: TChartType );
    begin
      _chartType := val;
      if( CTPdf = _chartType ) then yUnits := UnitsUnitless;
    end
  ;

  function TFrameFunctionEditor.getChartType(): TChartType; begin result := _chartType; end;

  // Properties to be passed through to the chart points editor when necessary
  procedure TFrameFunctionEditor.setUnitsLocked( val: boolean ); begin _unitsLocked := val; end;
  procedure TFrameFunctionEditor.setXUnits( val: TChartUnitType ); begin _xUnits := val; end;

  procedure TFrameFunctionEditor.setYUnits( val: TChartUnitType );
    begin
      if( CTPdf = _chartType ) then
        _yUnits := UnitsUnitless
      else
        _yUnits := val
      ;
    end
  ;

  procedure TFrameFunctionEditor.setMinY( val: double ); begin _minY := val; end;
  procedure TFrameFunctionEditor.setMaxY( val: double ); begin _maxY := val; end;


  function TFrameFunctionEditor.getUnitsLocked(): boolean; begin result := _unitsLocked; end;
  function TFrameFunctionEditor.getXUnits(): TChartUnitType; begin result := _xUnits; end;
  function TFrameFunctionEditor.getYUnits(): TChartUnitType; begin result := _yUnits; end;
  function TFrameFunctionEditor.getMinY(): double; begin result := _minY; end;
  function TFrameFunctionEditor.getMaxY(): double; begin result := _maxY; end;
//-----------------------------------------------------------------------------



end.
