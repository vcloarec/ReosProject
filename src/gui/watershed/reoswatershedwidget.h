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
class ReosWatershedModule;
class ReosWatershed;

class ReosWatershedWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosWatershedWidget( ReosMap *map, ReosWatershedModule *module, QWidget *parent = nullptr );
    ~ReosWatershedWidget();

    void setModel( ReosWatershedItemModel *model );

  private slots:
    void onWatershedAdded( const QModelIndex &index );
    void onButtonDelineateClicked();
    void onCurrentWatershedChange( const QItemSelection &selected, const QItemSelection &deselected );

  private:
    Ui::ReosWatershedWidget *ui;
    ReosWatershedItemModel *mModelWatershed = nullptr;
    ReosMap *mMap = nullptr;
    QAction *mActionDelineateWatershed = nullptr;
    ReosDelineatingWatershedWidget *mDelineatingWidget = nullptr;

    QMap<ReosWatershed *, ReosMapPolygon> mMapWatersheds;
    ReosMapMarker mCurrentMapOutlet;

    //! Method use for styling watershed polygon
    ReosMapPolygon formatWatershedPolygon( ReosMapPolygon &watershedPolygon );
    void clearSelection();
};

#endif // REOSWATERSHEDWIDGET_H
