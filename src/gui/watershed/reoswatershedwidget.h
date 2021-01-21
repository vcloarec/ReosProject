#ifndef REOSWATERSHEDWIDGET_H
#define REOSWATERSHEDWIDGET_H

#include <QWidget>
class QItemSelection;

#include "reosmap.h"

namespace Ui
{
  class ReosWatershedWidget;
}

class ReosWatershedItemModel;
class ReosDelineatingWatershedWidget;
class ReosLongitudinalProfileWidget;
class ReosWatershedModule;
class ReosWatershed;
class ReosMapToolSelectMapItem;

class ReosWatershedWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosWatershedWidget( ReosMap *map, ReosWatershedModule *module, QWidget *parent = nullptr );
    ~ReosWatershedWidget();

    void setModel( ReosWatershedItemModel *model );

  signals:
    void currentWatershedChanged( ReosWatershed *ws );

  private slots:
    void onWatershedAdded( const QModelIndex &index );
    void onWatershedSelectedOnMap( ReosMapItem *item, const QPointF &pos );
    void onRemoveWatershed();
    void onCurrentWatershedChange( const QItemSelection &selected, const QItemSelection &deselected );
    void onWatershedDataChanged( const QModelIndex &index );
    void onModuleReset();

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

    QMap<ReosWatershed *, ReosMapPolygon> mMapWatersheds;
    ReosMapMarker mCurrentMapOutlet;
    ReosMapPolyline mCurrentStreamLine;

    //! Method use for styling watershed polygon
    ReosMapPolygon &formatWatershedPolygon( ReosMapPolygon &&watershedPolygon );
    void clearSelection();
};

#endif // REOSWATERSHEDWIDGET_H
