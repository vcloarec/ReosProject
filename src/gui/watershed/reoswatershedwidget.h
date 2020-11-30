#ifndef REOSWATERSHEDWIDGET_H
#define REOSWATERSHEDWIDGET_H

#include <QWidget>

#include "reosmap.h"

namespace Ui
{
  class ReosWatershedWidget;
}

class ReosWatershedItemModel;
class ReosDelineatingWatershedWidget;
class ReosWatershedModule;

class ReosWatershedWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosWatershedWidget( ReosMap *map, ReosWatershedModule *module, QWidget *parent = nullptr );
    ~ReosWatershedWidget();

    void setModel( ReosWatershedItemModel *model );

  private slots:
    void onWatershedAdded( const QModelIndex &index );
    void updateMapWatershed();

    void onButtonDelineateClicked();

  private:
    Ui::ReosWatershedWidget *ui;
    ReosWatershedItemModel *mModelWatershed = nullptr;
    ReosMap *mMap = nullptr;

    ReosDelineatingWatershedWidget *mDelineatingWidget = nullptr;

    QList<ReosMapPolygon> mMapWatersheds;
    ReosMapPolygon formatWatershedPolygon( ReosMapPolygon &watershedPolygon );
};

#endif // REOSWATERSHEDWIDGET_H
