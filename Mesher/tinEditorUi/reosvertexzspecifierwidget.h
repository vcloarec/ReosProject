/***************************************************************************
                      reosvertexzspecifierwidget.h
                     --------------------------------------
Date                 : 01-10-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSVERTEXZSPECIFIERWIDGET_H
#define REOSVERTEXZSPECIFIERWIDGET_H


#include <QDialog>
#include <QLineEdit>
#include <QWidget>


#include "../ReosMesh/reosvertexzspecifier.h"
#include "../../Reos/Form/reosform.h"
#include "../../GIS/reosmaptool.h"
#include "../ReosMesh/reosmeshgenerator.h"
#include "reosmapmeshitem.h"
#include "../../GIS/reosmap.h"

class ReosVertexZSpecifierEntryWidgetModel;



namespace Ui
{
  class ReosVertexZSpecifierWidget;
}


class ReosVertexZSpecifierEntryWidget: public QWidget
{
  public:
    ReosVertexZSpecifierEntryWidget( QWidget *parent );
    virtual ~ReosVertexZSpecifierEntryWidget();

    virtual QIcon icon() const = 0;
    virtual QString description() const = 0;
    virtual ReosVertexZSpecifier::Type type() const = 0;
    virtual ReosVertexZSpecifierFactory &factory() = 0;
    virtual void assignZSpecifier( VertexPointer vert );

    //* with a existant Z Specifier, set the parameters of the widget
    virtual void setSpecifier( ReosVertexZSpecifier *specifier ) = 0;

    virtual void start();
    virtual void stop();
    virtual void clear() = 0;

    virtual void vertexHasToBeRemoved( VertexPointer vertex ) {( void )vertex;}
};

class ReosVertexZSpecifierSimpleValueWidget: public ReosVertexZSpecifierEntryWidget
{
    Q_OBJECT
  public:
    ReosVertexZSpecifierSimpleValueWidget( QWidget *parent );

    QIcon icon()  const override;
    QString description() const override {return tr( "Value" );}
    ReosVertexZSpecifier::Type type() const override {return ReosVertexZSpecifier::Type::Simple;}
    virtual void clear() override
    {
      zValueParameterForm->setValue( 0 );
      mFactory.setZValue( 0 );
    }

    ReosVertexZSpecifierFactory &factory() override;

    virtual void setSpecifier( ReosVertexZSpecifier *specifier ) override
    {
      if ( specifier->type() != type() )
        return;

      auto simpleSpecifier = static_cast<ReosVertexZSpecifierSimple *>( specifier );
      zValueParameterForm->setValue( simpleSpecifier->zValue() );
    }

  private slots:
    void ZValueHasBeenEdited();
  private:
    ReosVertexZSpecifierSimpleFactory mFactory;
    ReosForm *valueForm;
    ReosFormParameterSimpleDouble *zValueParameterForm;
};

class ReosVertexZSpecifierSelectReferenceMapTool: public ReosMapToolSelection
{
    Q_OBJECT
  public:
    ReosVertexZSpecifierSelectReferenceMapTool( ReosMap *map, ReosMapMeshEditorItemDomain *domain );

    void start();
    void returnToPreviousMapTool();

    // QgsMapTool interface
  public:
    void canvasMoveEvent( QgsMapMouseEvent *e ) override;

  private:
    ReosMapMeshEditorItemDomain *mDomain;
    ReosMapTool *mPreviousCurrentMapTool = nullptr;

    // ReosMapTool interface
  public slots:
    void askForEscape() override;
};

class ReosVertexZSpecifierDependentOtherVertexWidget: public ReosVertexZSpecifierEntryWidget
{
    Q_OBJECT
  public:
    ReosVertexZSpecifierDependentOtherVertexWidget( ReosMap *map, ReosMapMeshEditorItemDomain *domain, QWidget *parent );

    void init();

    void disableTakeLastVertex()
    {
      mTakeNewVertexAsReference->hideWidget();
    }

    ReosVertexZSpecifierFactory &factory() override;

    void start() override;
    void stop() override;

    virtual void clear() override
    {
      clearVertexReference();
      mValueParameterForm->setValue( 0 );
    }

    virtual void vertexHasToBeRemoved( VertexPointer vertex ) override;

    void assignZSpecifier( VertexPointer vert ) override;

    void setValue( double value )
    {
      mValueParameterForm->setValue( value );
    }

    void setOtherVertex( VertexPointer vertex )
    {
      setVertexReference( static_cast<ReosMeshItemVertex *>( vertex->graphicPointer() ) );
      updateReferenceText();
    }

  private slots:
    void startSelectReference();
    void updateReferenceText();
    void zoneHasBeenSelected( const QRectF &rect );

    virtual void valueHasBeenEdited();

  private:

    virtual void setValueInFactory( double value ) = 0;
    virtual ReosFormParameterSimpleDouble *makeValueParameterForm( ReosForm *parentForm ) = 0;
    void setVertexReference( ReosMeshItemVertex *vert );
    void clearVertexReference();
    void hideVertexReference();

    virtual ReosVertexZSpecifierDependOnOtherVertexFactory &dependentVertexFactory() = 0;
    ReosForm *mValueForm;
    ReosFormAction *mSelectReferenceAction;
    ReosFormText *mReferenceText;
    ReosFormParameterSimpleDouble *mValueParameterForm;
    ReosFormParameterSimpleBool *mTakeNewVertexAsReference;

    ReosMap *mMap;
    ReosMapMeshEditorItemDomain *mDomain;

    ReosVertexZSpecifierSelectReferenceMapTool *selectReferenceMapTool = nullptr;


};

class ReosVertexZSpecifierSlopeWidget: public ReosVertexZSpecifierDependentOtherVertexWidget
{

  public:
    ReosVertexZSpecifierSlopeWidget( ReosMap *map, ReosMapMeshEditorItemDomain *domain, QWidget *parent );
    QIcon icon() const override;
    QString description() const override {return tr( "Other vertex and slope" );}
    ReosVertexZSpecifier::Type type() const override {return ReosVertexZSpecifier::Type::VertexAndSlope;}
    virtual void clear() override;

    virtual void setSpecifier( ReosVertexZSpecifier *specifier ) override;
  private:

    ReosVertexZSpecifierDependOnOtherVertexFactory &dependentVertexFactory() override {return mFactory;}
    virtual ReosFormParameterSimpleDouble *makeValueParameterForm( ReosForm *parentForm ) override;
    virtual void setValueInFactory( double value ) override;

    ReosVertexZSpecifierOtherVertexAndSlopeFactory mFactory;
};

class ReosVertexZSpecifierGapWidget: public ReosVertexZSpecifierDependentOtherVertexWidget
{

  public:
    ReosVertexZSpecifierGapWidget( ReosMap *map, ReosMapMeshEditorItemDomain *domain, QWidget *parent );
    QIcon icon() const override;
    QString description() const override {return tr( "Other vertex and gap" );}
    ReosVertexZSpecifier::Type type() const override {return ReosVertexZSpecifier::Type::VertexAndGap;}
    virtual void clear() override;

    virtual void setSpecifier( ReosVertexZSpecifier *specifier ) override;

  private:

    ReosVertexZSpecifierDependOnOtherVertexFactory &dependentVertexFactory() override;
    virtual ReosFormParameterSimpleDouble *makeValueParameterForm( ReosForm *parentForm ) override;
    virtual void setValueInFactory( double value ) override;

    ReosVertexZSpecifierOtherVertexAndGapFactory mFactory;

};

class ReosVertexZSpecifierInterpolationWidget: public ReosVertexZSpecifierEntryWidget
{
    Q_OBJECT
  public:
    ReosVertexZSpecifierInterpolationWidget( ReosMap *map, ReosMapMeshEditorItemDomain *domain, QWidget *parent );
    QIcon icon() const override;
    QString description() const override {return tr( "Interpolation" );}
    ReosVertexZSpecifier::Type type() const override {return ReosVertexZSpecifier::Type::Interpolator;}

    virtual void clear() override;

    virtual void setSpecifier( ReosVertexZSpecifier *specifier ) override;

    ReosVertexZSpecifierFactory &factory() override;
    void start() override;
    void stop() override;

    void assignZSpecifier( VertexPointer vert ) override;

    virtual void vertexHasToBeRemoved( VertexPointer vertex ) override;

  private slots:
    void updateReferencesText();
    void startSelectReferences();

    void zoneHasBeenSelected( const QRectF &zone );

  private:
    void updateInterpolationLine();
    void showExtremityReferences();
    void clearExtremityReferences();
    void hideExtremityReferences();

    ReosForm *mForm;
    ReosFormAction *mSelectReferenceAction;
    ReosFormText *mFirstReferenceText;
    ReosFormText *mSecondReferenceText;

    ReosVertexZSpecifierInterpolationFactory mFactory;

    //! line between the interpolated points
    ReosMapItemPolyline *mInterpolationLine;

    ReosMap *mMap;
    ReosMapMeshEditorItemDomain *mDomain;

    ReosVertexZSpecifierSelectReferenceMapTool *selectReferenceMapTool = nullptr;


};



class ReosVertexZSpecifierWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosVertexZSpecifierWidget( ReosMap *map, ReosMapMeshEditorItemDomain *domain, QWidget *parent = nullptr );
    ~ReosVertexZSpecifierWidget();

    void assignZSpecifier( VertexPointer vert );

    void start();
    void stop();

  public slots:
    void vertexHasToBeRemoved( VertexPointer vert )
    {
      for ( auto e : mEntryWidgets )
        e->vertexHasToBeRemoved( vert );
    }

  private slots:
    void listViewClicked( QModelIndex index );

  private:

    void setCurrentZSpecifier( int i );
    void addEntry( ReosVertexZSpecifierEntryWidget *entry );

    Ui::ReosVertexZSpecifierWidget *ui;
    QList<ReosVertexZSpecifierEntryWidget *> mEntryWidgets;
    ReosVertexZSpecifierEntryWidgetModel *mEntriesModel;
    ReosVertexZSpecifierEntryWidget *mCurrentEntryWidget = nullptr;
};



class ReosVertexZSpecifierEntryWidgetModel: public QAbstractListModel
{
  public:
    ReosVertexZSpecifierEntryWidgetModel( QList<ReosVertexZSpecifierEntryWidget *> &entriesList, QObject *parent = nullptr );

    // QAbstractItemModel interface
  public:
    int rowCount( const QModelIndex &parent ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;

    void setTextDisplayed( bool b )
    {
      mTextDisplayed = b;
    }

  private:
    QList<ReosVertexZSpecifierEntryWidget *> &mEntriesList;

    bool mTextDisplayed = false;

};

QString vertexReferenceText( VertexPointer vert );


#endif // REOSVERTEXZSPECIFIERWIDGET_H
