#ifndef REOSHECRASSIMULATIONEDITWIDGET_H
#define REOSHECRASSIMULATIONEDITWIDGET_H

#include <QWidget>

#include "reoshydraulic2dsimulationwidget.h"

class ReosHecRasSimulation;

namespace Ui
{
  class ReosHecRasSimulationEditWidget;
}

class ReosHecRasSimulationEditWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosHecRasSimulationEditWidget(ReosHecRasSimulation *simulation, QWidget *parent = nullptr );
    ~ReosHecRasSimulationEditWidget();

  private slots:
    void onPlanChanged();

  private:
    Ui::ReosHecRasSimulationEditWidget *ui;
    ReosHecRasSimulation *mSimulation = nullptr;
};



class ReosHecRasSimulationEditWidgetFactory : public ReosHydraulicSimulationWidgetFactory
{
  public:
    QString key() const override;

    QWidget *simulationSettingsWidget( ReosHydraulicStructure2D *structure, ReosHydraulicSimulation *simulation, const ReosGuiContext &guiContext ) const override;
    QDialog *engineConfigurationDialog( QWidget *parent ) const override { return nullptr; }
    QWidget *simulationEngineDescription( QWidget *parent ) const override { return nullptr; }
    ReosImportHydraulicStructureWidget *simulationImportWidget( QWidget *parent ) const override;
};

#endif // REOSHECRASSIMULATIONEDITWIDGET_H
