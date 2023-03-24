#ifndef REOSADDHYDROGRAPHNODEFROMWIDGET_H
#define REOSADDHYDROGRAPHNODEFROMWIDGET_H

#include <QWidget>
#include "reosactionwidget.h"

class ReosGuiContext;
class ReosMap;
class ReosDataProviderSelectorWidget;
class ReosHydraulicNetwork;
class ReosSpatialPosition;
class ReosHydrograph;

namespace Ui
{
  class ReosAddHydrographNodeFromWidget;
}

class ReosAddHydrographNodeFromWidget : public ReosActionWidget
{
    Q_OBJECT

  public:
    explicit ReosAddHydrographNodeFromWidget( ReosHydraulicNetwork *network, const ReosGuiContext &context );
    ~ReosAddHydrographNodeFromWidget();

  private slots:
    void onProviderChanged();
    void onAddNodeCopy();
    void onAddNode();

  private:
    Ui::ReosAddHydrographNodeFromWidget *ui;
    ReosMap *mMap = nullptr;
    ReosHydraulicNetwork *mNetWork = nullptr;
    ReosDataProviderSelectorWidget *mCurrentWidget = nullptr;
    bool mIsDatasetSelected = false;
    bool mIsDataReady = false;

    void addNode( const QVariantMap &metadata, ReosHydrograph *hydrograph );
};

#endif // REOSADDHYDROGRAPHNODEFROMWIDGET_H
