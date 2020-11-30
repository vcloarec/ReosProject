#ifndef REOSWATERSHEDWIDGET_H
#define REOSWATERSHEDWIDGET_H

#include <QWidget>

#include "reosmap.h"

namespace Ui
{
  class ReosWatershedWidget;
}

class ReosWatershedItemModel;

class ReosWatershedWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosWatershedWidget( ReosMap *map, QWidget *parent = nullptr );
    ~ReosWatershedWidget();

    void setModel( ReosWatershedItemModel *model );

  private slots:
    void onWatershedAdded( const QModelIndex &index );
    void updateMapWatershed();

  private:
    Ui::ReosWatershedWidget *ui;
    ReosWatershedItemModel *mModelWatershed = nullptr;
    ReosMap *mMap = nullptr;

    QList<ReosMapPolygon> mMapWatersheds;

    ReosMapPolygon formatWatershedPolygon( ReosMapPolygon &watershedPolygon );
};

#endif // REOSWATERSHEDWIDGET_H
