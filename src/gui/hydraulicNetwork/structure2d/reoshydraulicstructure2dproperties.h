/***************************************************************************
  reoshydraulicstructrure2dproperties.h - ReosHydraulicStructrure2DProperties

 ---------------------
 begin                : 10.1.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSHYDRAULICSTRUCTURE2DPROPERTIES_H
#define REOSHYDRAULICSTRUCTURE2DPROPERTIES_H

#include <QWidget>
#include <QPointer>

#include "reoshydraulicelementpropertieswidget.h"
#include "reoshydraulicstructure2d.h"
#include "reoshydraulicnetworkwidget.h"
#include "reoscalculationcontext.h"


class Reos3dView;
class ReosColorButton;
class ReosVariableTimeStepPlotListButton;
class DatasetSettingsWidgetAction;

namespace Ui
{
  class ReosHydraulicStructure2DProperties;
}

class ReosHydraulicStructure2DProperties : public ReosHydraulicElementWidget
{
    Q_OBJECT

  public:
    explicit ReosHydraulicStructure2DProperties( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &context = ReosGuiContext() );
    ~ReosHydraulicStructure2DProperties();

    void setCurrentCalculationContext( const ReosCalculationContext &context ) override;
    void setTime( const QDateTime &time ) override;

    QAction *action3DView() const;
    QAction *actionEditStructure() const;
    QAction *actionRunSimulation() const;
    QAction *scalarWidgetAction() const;
    QAction *vectorWidgetAction() const;
    QAction *actionProfiles() const;
    QAction *actionExportAsMesh() const;

  signals:
    void calculationContextChanged();

  public slots:
    void updateProgress();

  private slots:
    void requestMapRefresh();
    void onLaunchCalculation();
    void onExportSimulation();
    void updateDatasetMenus();
    void populateHydrograph();
    void onSimulationFinished();
    void onMapCursorMove( const QPointF &pos );
    void restoreResults();
    void initialize3DView();

  private:
    Ui::ReosHydraulicStructure2DProperties *ui;
    ReosHydraulicStructure2D *mStructure2D = nullptr;
    QPointer<ReosMap> mMap = nullptr;
    QAction *mActionEditStructure = nullptr;
    QAction *mActionRunSimulation = nullptr;
    QAction *mActionExportSimulationFile = nullptr;
    QAction *mActionEngineConfiguration = nullptr;
    QAction *mAction3DView = nullptr;
    QAction *mActionProfiles = nullptr;
    QAction *mActionExportAsMesh = nullptr;
    QMenu *mScalarDatasetMenu = nullptr;
    DatasetSettingsWidgetAction *mScalarWidgetAction = nullptr;
    QMenu *mVectorDatasetMenu = nullptr;
    DatasetSettingsWidgetAction *mVectorWidgetAction = nullptr;
    QActionGroup *mScalarDatasetActions = nullptr;
    QActionGroup *mVectorDatasetActions = nullptr;
    QAction *mActionScalarSettings = nullptr;
    QAction *mActionVectorSettings = nullptr;
    QToolButton *mDatasetVectorSettingsButton = nullptr;
    QPointer<Reos3dView> mView3D;
    ReosGuiContext mGuiContext;
    ReosCalculationContext mCalculationContext;
    ReosVariableTimeStepPlotListButton *mInputHydrographPlotButton = nullptr;
    ReosVariableTimeStepPlotListButton *mOutputHydrographPlotButton = nullptr;

    QPointer<ReosSimulationProcess> mCurrentProcess;
    void setCurrentSimulationProcess( ReosSimulationProcess *process, const ReosCalculationContext &context );

    void disableResultGroupBox();
    void fillResultGroupBox( const ReosCalculationContext &context );
    void updateScalarDatasetMenu();
    void updateVectorDatasetMenu();

    QString mCurrentDatasetId;
    QString mCurrentVectorDatasetId;

};


class ReosHydraulicStructure2DPropertiesWidgetFactory : public ReosHydraulicElementWidgetFactory
{
  public:
    ReosHydraulicStructure2DPropertiesWidgetFactory( QObject *parent = nullptr ): ReosHydraulicElementWidgetFactory( parent ) {}
    virtual ReosHydraulicElementWidget *createWidget( ReosHydraulicNetworkElement *element, const ReosGuiContext &context = ReosGuiContext() );
    virtual QString elementType() {return ReosHydraulicStructure2D::staticType();}
};


class ReosMeshWireframeSettingsWidget: public QWidget
{
    Q_OBJECT
  public:
    ReosMeshWireframeSettingsWidget( QWidget *parent = nullptr );

    void setSettings( const ReosMesh::WireFrameSettings &settings );
    ReosMesh::WireFrameSettings settings() const;

  signals:
    void changed();

  private:
    QCheckBox *mEnableWireframeCheckBox = nullptr;
    ReosColorButton *mColorButton = nullptr;
    QSlider *mWidthSlider = nullptr;
};

#endif // REOSHYDRAULICSTRUCTURE2DPROPERTIES_H
