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

#include <QWidget>
#include <QLineEdit>

#include "../ReosMesh/reosvertexzspecifier.h"
#include "../../Reos/Form/reosform.h"
#include "../../GIS/reosmaptool.h"
#include "../ReosMesh/reosmeshgenerator.h"
#include "reosmapmeshitem.h"
#include "../../GIS/reosmap.h"

class ReosVertexZSpecifierEntryWidgetModel;

static void setVertexReferenceText( ReosFormText *formText, VertexPointer vert );

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
    virtual ReosVertexZSpecifierFactory &factory() = 0;
    virtual void assignZSpecifier( VertexPointer vert );

    virtual void start();
    virtual void stop();

    virtual void vertexHasToBeRemoved( VertexPointer vertex ) {( void )vertex;}
};

class ReosVertexZSpecifierSimpleValueWidget: public ReosVertexZSpecifierEntryWidget
{
    Q_OBJECT
  public:
    ReosVertexZSpecifierSimpleValueWidget( QWidget *parent );

    QIcon icon() const;

    ReosVertexZSpecifierFactory &factory();

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

    ReosVertexZSpecifierFactory &factory() override;

    void start() override;
    void stop() override;

    virtual void vertexHasToBeRemoved( VertexPointer vertex ) override
    {
      if ( vertex == dependentVertexFactory().otherVertex() )
      {
        clearVertexReference();
      }
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


    // ReosVertexZSpecifierEntryWidget interface
  public:
    void assignZSpecifier( VertexPointer vert ) override;
};

class ReosVertexZSpecifierSlopeWidget: public ReosVertexZSpecifierDependentOtherVertexWidget
{

  public:
    ReosVertexZSpecifierSlopeWidget( ReosMap *map, ReosMapMeshEditorItemDomain *domain, QWidget *parent );
    QIcon icon() const override;

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
    ReosVertexZSpecifierFactory &factory() override;
    void start() override;
    void stop() override;

    void assignZSpecifier( VertexPointer vert ) override;

    virtual void vertexHasToBeRemoved( VertexPointer vertex ) override
    {
      if ( vertex == mFactory.firstExtremity() || vertex == mFactory.secondExtremity() )
      {
        clearExtremityReferences();
      }
    }

  private slots:
    void updateReferencesText();
    void startSelectReferences()
    {
      clearExtremityReferences();
      updateInterpolationLine();
      mInterpolationLine->hide();
      updateReferencesText();
      selectReferenceMapTool->start();
    }

    void zoneHasBeenSelected( const QRectF &zone );

  private:
    void updateInterpolationLine();
    void showExtremityReferences()
    {
      if ( mFactory.firstExtremity() )
        static_cast<ReosMeshItemVertex *>( mFactory.firstExtremity()->graphicPointer() )->setReference( true );
      if ( mFactory.secondExtremity() )
        static_cast<ReosMeshItemVertex *>( mFactory.secondExtremity()->graphicPointer() )->setReference( true );
    }
    void clearExtremityReferences()
    {
      hideExtremityReferences();
      mFactory.setExtremitiesVertices( nullptr, nullptr );
    }

    void hideExtremityReferences()
    {
      if ( mFactory.firstExtremity() )
        static_cast<ReosMeshItemVertex *>( mFactory.firstExtremity()->graphicPointer() )->setReference( false );
      if ( mFactory.secondExtremity() )
        static_cast<ReosMeshItemVertex *>( mFactory.secondExtremity()->graphicPointer() )->setReference( false );
    }

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

  private:
    QList<ReosVertexZSpecifierEntryWidget *> &mEntriesList;

};

#endif // REOSVERTEXZSPECIFIERWIDGET_H
