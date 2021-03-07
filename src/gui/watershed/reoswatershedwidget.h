#ifndef REOSWATERSHEDWIDGET_H
#define REOSWATERSHEDWIDGET_H

#include <QWidget>
class QItemSelection;

#include "reosgui.h"
#include "reosmap.h"

namespace Ui
{
  class ReosWatershedWidget;
}

class ReosWatershedItemModel;
class ReosDelineatingWatershedWidget;
class ReosLongitudinalProfileWidget;
class ReosConcentrationTimeWidget;
class ReosWatershedModule;
class ReosWatershed;
class ReosMapToolSelectMapItem;
class ReosMapToolEditMapPolygon;
class ReosMeteorologicItemModel;
class ReosMeteorologicModelWidget;
class ReosRunoffHydrographWidget;

class REOSGUI_EXPORT ReosWatershedWidget : public QWidget
{
    Q_OBJECT
  public:
    explicit ReosWatershedWidget( ReosMap *map, ReosWatershedModule *module, QWidget *parent = nullptr );
    ~ReosWatershedWidget();

  signals:
    void currentWatershedChanged( ReosWatershed *ws );

  private slots:
    void onWatershedAdded( const QModelIndex &index );
    void onWatershedSelectedOnMap( ReosMapItem *item, const QPointF &pos );
    void onRemoveWatershed();
    void onCurrentWatershedChange( const QItemSelection &selected, const QItemSelection &deselected );
    void onWatershedDataChanged( const QModelIndex &index );
    void onModuleReset();
    void onExportToVectorLayer();

  private:
    Ui::ReosWatershedWidget *ui;
    ReosWatershedItemModel *mModelWatershed = nullptr;

    ReosMap *mMap = nullptr;

    QAction *mActionSelectWatershed = nullptr;
    ReosMapToolSelectMapItem *mMapToolSelectWatershed = nullptr;
    QAction *mActionRemoveWatershed = nullptr;

    QAction *mActionDelineateWatershed = nullptr;
    ReosDelineatingWatershedWidget *mDelineatingWidget = nullptr;

    QAction *mActionLongitudinalProfile = nullptr;
    ReosLongitudinalProfileWidget *mLongitudinalProfileWidget = nullptr;

    QAction *mActionConcentrationTime = nullptr;
    ReosConcentrationTimeWidget *mConcentrationTimeWidget = nullptr;

    QAction *mActionMeteorologicModel = nullptr;
    ReosMeteorologicModelWidget *mMeteorolocicModelWidget = nullptr;

    QAction *mActionRunoffHydrograph = nullptr;
    ReosRunoffHydrographWidget *mRunoffHydrographWidget = nullptr;

    QAction *mActionExportToVectorLayer = nullptr;

    using MapWatersheds = std::map<ReosWatershed *, std::unique_ptr<ReosMapPolygon>>;
    MapWatersheds mMapWatersheds;

    ReosMapMarker mCurrentMapOutlet;
    ReosMapPolyline mCurrentStreamLine;

    ReosMapToolEditMapPolygon *mMapToolEditDelineating = nullptr;

    ReosWatershed *currentWatershed() const;
    void formatWatershedPolygon( ReosMapPolygon * );
    void clearSelection();

    void setWatershedModel( ReosWatershedItemModel *model );
};

#endif // REOSWATERSHEDWIDGET_H
