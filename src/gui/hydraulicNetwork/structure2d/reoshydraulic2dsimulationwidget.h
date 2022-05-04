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

class ReosHydraulicSimulation;
class ReosHydraulicStructure2D;

namespace Ui
{
  class ReosHydraulic2DSimulationWidget;
}

class ReosHydraulic2DSimulationWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosHydraulic2DSimulationWidget( ReosHydraulicStructure2D *structure, QWidget *parent = nullptr );
    ~ReosHydraulic2DSimulationWidget();

  private slots:
    void onAddSimulation();
    void onSimulationIndexChanged( int newIndex );

  private:
    Ui::ReosHydraulic2DSimulationWidget *ui;
    ReosHydraulicStructure2D *mStructure = nullptr;
    QWidget *mCurrentEditingWidget = nullptr;

    void setCurrentSimulation( ReosHydraulicSimulation *simulation );
    void updateSimulationCombo();
};


class ReosHydraulicSimulationWidgetFactory
{
  public:
    virtual QString key() const = 0;
    virtual QWidget *simulationSettingsWidget( ReosHydraulicStructure2D *structure, ReosHydraulicSimulation *simulation, QWidget *parent ) const = 0;
    virtual QDialog *engineConfigurationDialog( QWidget *parent ) const = 0;
};


class ReosHydraulicSimulationWidgetRegistery
{
  public:
    ReosHydraulicSimulationWidgetRegistery();

    //! Creates and returns a simuation corresponding to the \a key
    QWidget *createEditingWidget( ReosHydraulicStructure2D *structure,  ReosHydraulicSimulation *simulation, QWidget *parent );

    //! Creates and returns an engine configuration dialog corresponding to the \a key
    QDialog *createConfigurationDialog( const QString &key, QWidget *parent );

    //! Returns a pointer to the static instance of this registery
    static ReosHydraulicSimulationWidgetRegistery *instance();

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
