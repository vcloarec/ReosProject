/***************************************************************************
  reoshydraulic2dsimulationwidget.h - ReosHydraulic2DSimulationWidget

 ---------------------
 begin                : 29.3.2022
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
#ifndef REOSHYDRAULIC2DSIMULATIONWIDGET_H
#define REOSHYDRAULIC2DSIMULATIONWIDGET_H

#include <QWidget>
#include <memory>

#include "reosguicontext.h"

class ReosHydraulicSimulation;
class ReosHydraulicStructure2D;
class ReosImportHydraulicStructureWidget;

namespace Ui
{
  class ReosHydraulic2DSimulationWidget;
}

class ReosHydraulic2DSimulationWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosHydraulic2DSimulationWidget( ReosHydraulicStructure2D *structure, const ReosGuiContext &guiContext );
    ~ReosHydraulic2DSimulationWidget();

  private slots:
    void onAddSimulation();
    void onRemovedSimulation();
    void onSimulationIndexChanged( int newIndex );

  private:
    Ui::ReosHydraulic2DSimulationWidget *ui;
    ReosHydraulicStructure2D *mStructure = nullptr;
    QWidget *mCurrentEditingWidget = nullptr;
    ReosGuiContext mGuiContext;

    void setCurrentSimulation( ReosHydraulicSimulation *simulation );
    void updateSimulationCombo();
};


class ReosHydraulicSimulationWidgetFactory
{
  public:
    ~ReosHydraulicSimulationWidgetFactory();

    virtual QString key() const = 0;
    virtual QWidget *simulationSettingsWidget( ReosHydraulicStructure2D *structure, ReosHydraulicSimulation *simulation, const ReosGuiContext &guiContext ) const = 0;
    virtual QDialog *engineConfigurationDialog( QWidget *parent ) const = 0;
    virtual QWidget *simulationEngineDescription( QWidget *parent ) const = 0;
    virtual ReosImportHydraulicStructureWidget* simulationImportWidget(QWidget* parent) const = 0;
};


class ReosHydraulicSimulationWidgetRegistery
{
  public:
    ReosHydraulicSimulationWidgetRegistery();

    //! Creates and returns a simuation corresponding to the \a key
    QWidget *createEditingWidget( ReosHydraulicStructure2D *structure,  ReosHydraulicSimulation *simulation, const ReosGuiContext &guiContext );

    //! Creates and returns an engine configuration dialog corresponding to the \a key
    QDialog *createConfigurationDialog( const QString &key, QWidget *parent );

    //! Creates and returns an engine descripton widget corresponding to the \a key
    QWidget *createDescription( const QString &key, QWidget *parent );

    //! Creates and returns an engine descripton widget corresponding to the \a key
    ReosImportHydraulicStructureWidget* createImportWidget(const QString& key, QWidget* parent);

    //! Returns a pointer to the static instance of this registery
    static ReosHydraulicSimulationWidgetRegistery *instance();

    //! Returns all the registered keys
    QStringList keys() const;

  private:
#ifdef _MSC_VER
    std::unique_ptr<ReosHydraulicSimulationWidgetFactory> dummy; // workaround for MSVC, if not, the line after create an compilation error if this class is exported (REOSCORE_EXPORT)
#endif
    //! Registers a \a factory
    void registerEngineFactory( ReosHydraulicSimulationWidgetFactory *factory );

    std::map<QString, std::unique_ptr<ReosHydraulicSimulationWidgetFactory>> mFactories;
    static ReosHydraulicSimulationWidgetRegistery *sInstance;
    void loadDynamicLibrary();
};


#endif // REOSHYDRAULIC2DSIMULATIONWIDGET_H
