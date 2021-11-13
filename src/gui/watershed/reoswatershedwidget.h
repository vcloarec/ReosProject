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
class ReosMapToolMoveMapItem;
class ReosMeteorologicItemModel;
class ReosMeteorologicModelWidget;
class ReosRunoffHydrographWidget;
class ReosGaugedHydrographWidget;

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
    void onTreeViewContextMenu( const QPoint &pos );
    void onWatershedDataChanged( const QModelIndex &index );
    void onModuleReset();
    void onExportToVectorLayer();
    void onZoomToWatershed();

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

    QAction *mActionGaugedHydrograph = nullptr;
    ReosGaugedHydrographWidget *mGaugedHydrographWidget = nullptr;

    QAction *mActionExportToVectorLayer = nullptr;

    QAction *mActionZoomToWatershed = nullptr;

    struct MapWatershed
    {
      MapWatershed() {}
      MapWatershed( ReosMap *map, const QPolygonF &delineat, const QPointF &outletPt )
      {
        delineating = std::make_shared<ReosMapPolygon>( map, delineat );
        outletPoint = std::make_shared<ReosMapMarkerFilledCircle>( map, outletPt );
      }

      std::shared_ptr<ReosMapPolygon> delineating;
      std::shared_ptr<ReosMapMarkerFilledCircle> outletPoint;
    };

    using MapWatersheds = QMap<ReosWatershed *, MapWatershed>;
    MapWatersheds mMapWatersheds;

    ReosMapPolyline mCurrentStreamLine;

    ReosMapToolEditMapPolygon *mMapToolEditDelineating = nullptr;
    ReosMapToolMoveMapItem *mMapToolMoveOutletPoint = nullptr;

    ReosWatershed *currentWatershed() const;
    void formatMapWatershed( MapWatershed &mapWatershed );
    void formatSelectedWatershed( MapWatershed &mapWatershed );
    void formatUnselectedWatershed( MapWatershed &mapWatershed );
    void clearSelection();

    void setWatershedModel( ReosWatershedItemModel *model );

    ReosMapPolygon *mapDelineating( ReosWatershed *ws );
};

#endif // REOSWATERSHEDWIDGET_H
