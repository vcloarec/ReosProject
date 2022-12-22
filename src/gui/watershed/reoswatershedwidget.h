#ifndef REOSWATERSHEDWIDGET_H
#define REOSWATERSHEDWIDGET_H

#include <QWidget>
class QItemSelection;

#include "reosgui.h"
#include "reosmap.h"
#include "reosdockwidget.h"

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
class ReosWatershedGaugedHydrographWidget;
class ReosHydraulicNetwork;
class ReosHydrographNodeWatershed;
class ReosGuiContext;
class ReosTimeWindow;

class REOSGUI_EXPORT ReosWatershedWidget : public QWidget
{
    Q_OBJECT
  public:
    explicit ReosWatershedWidget( const ReosGuiContext &guiContext,
                                  ReosWatershedModule *module,
                                  ReosHydraulicNetwork *hydraulicNetwork = nullptr,
                                  ReosDockWidget *parent = nullptr );
    ~ReosWatershedWidget();

    ReosTimeWindow timeWindow() const;
    ReosDuration mapTimeStep() const;

    QAction *meteorologicalModelAction() const;
    QAction *displayGriddedPrecipitationOnMap() const;

    const QString &descriptionKeyWatershed() const;

  signals:
    void currentWatershedChanged( ReosWatershed *ws );
    void timeWindowChanged();
    void mapTimeStepChanged();

  private slots:
    void onWatershedAdded( const QModelIndex &index );
    void onWatershedSelectedOnMap( ReosMapItem *item, const QPointF &pos );
    void onRemoveWatershed();
    void onCurrentWatershedChanges( const QItemSelection &selected, const QItemSelection &deselected );
    void onTreeViewContextMenu( const QPoint &pos );
    void onWatershedDataChanged( const QModelIndex &index );
    void onModuleReset();
    void onExportToVectorLayer();
    void onZoomToWatershed();
    void onAddRemoveNetwork();

    void onClosed();
    void onOpened();

  private:
    Ui::ReosWatershedWidget *ui;
    ReosWatershedModule *mWatershdModule = nullptr;
    ReosWatershedItemModel *mModelWatershed = nullptr;
    ReosMap *mMap = nullptr;
    ReosHydraulicNetwork *mHydraulicNetwork = nullptr;

    QAction *mActionSelectWatershed = nullptr;
    QString mDescriptionKeyWatershed;
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
    ReosWatershedGaugedHydrographWidget *mGaugedHydrographWidget = nullptr;

    QAction *mActionExportToVectorLayer = nullptr;

    QAction *mActionZoomToWatershed = nullptr;

    struct MapWatershed
    {
      MapWatershed() {}
      MapWatershed( ReosMap *map, const QPolygonF &delineat, const QPointF &outletPt );

      void setVisible( bool b );

      std::shared_ptr<ReosMapPolygon> delineating;
      std::shared_ptr<ReosMapMarkerFilledCircle> outletPoint;
    };

    using MapWatersheds = QHash<ReosWatershed *, MapWatershed>;
    MapWatersheds mMapWatersheds;

    void constructMapWatershed( ReosWatershed *watershed );

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
    void updateNetworkButton();
    ReosHydrographNodeWatershed *currentNetworkNode() const;
    ReosHydrographNodeWatershed *associatedNetworkNode( ReosWatershed *watershed ) const;

    void setVisibleMapItems( bool visible );
};

class REOSGUI_EXPORT ReosWatershedDockWidget: public ReosDockWidget
{

  public:
    ReosWatershedDockWidget( const ReosGuiContext &context, ReosWatershedModule *module, ReosHydraulicNetwork *hydraulicNetwork = nullptr );;

    ReosWatershedWidget *watershedWidget() const;
    QAction *actionToggle() const;

  private:
    ReosWatershedWidget *mWatershedWidget = nullptr;
    QAction *mActionToggle = nullptr;
};

#endif // REOSWATERSHEDWIDGET_H
