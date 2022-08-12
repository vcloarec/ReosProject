#ifndef REOSHYDRAULICSTRUCTURERESULTEXPORT_H
#define REOSHYDRAULICSTRUCTURERESULTEXPORT_H

#include <QDialog>

namespace Ui
{
  class ReosHydraulicStructureResultExport;
}

class ReosHydraulicStructure2D;
class ReosHydraulicNetwork;
class ReosHydraulicScheme;

class ReosHydraulicStructureResultExport : public QDialog
{
    Q_OBJECT

  public:
    explicit ReosHydraulicStructureResultExport( ReosHydraulicStructure2D *structure, const QString &currentSchemeId, QWidget *parent = nullptr );
    ~ReosHydraulicStructureResultExport();

  public slots:
    void accept() override;

  private slots:
    void onCurrentSchemeChange();
    void onFileButtonClicked();
    void onQGISFileChanged();

  private:
    Ui::ReosHydraulicStructureResultExport *ui;
    ReosHydraulicStructure2D *mStructure = nullptr;
    ReosHydraulicNetwork *mNetwork = nullptr;
};

#endif // REOSHYDRAULICSTRUCTURERESULTEXPORT_H
