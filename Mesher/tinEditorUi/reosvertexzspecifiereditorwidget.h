#ifndef REOSVERTEXZSPECIFIEREDITORWIDGET_H
#define REOSVERTEXZSPECIFIEREDITORWIDGET_H

#include <QDialog>

#include "reosvertexzspecifierwidget.h"

namespace Ui
{
  class ReosVertexZSpecifierEditorWidget;
}

class ReosVertexZSpecifierEditorWidget : public QDialog
{
    Q_OBJECT

  public:
    explicit ReosVertexZSpecifierEditorWidget( ReosMap *map, ReosMapMeshEditorItemDomain *domain, QWidget *parent = nullptr );
    ~ReosVertexZSpecifierEditorWidget();

  public slots:
    void setVertex( VertexPointer vertex );

    void stop()
    {
      if ( mCurrentVertex )
        static_cast<ReosMeshItemVertex *>( mCurrentVertex->graphicPointer() )->setCurrent( false );
    }

  private slots:
    void currentEntryChanged( int index );

  private:
    Ui::ReosVertexZSpecifierEditorWidget *ui;

    QList<ReosVertexZSpecifierEntryWidget *> mEntryWidgets;
    ReosVertexZSpecifierEntryWidgetModel *mEntriesModel;
    ReosVertexZSpecifierEntryWidget *mCurrentEntryWidget = nullptr;

    ReosFormText *mVertexReference;
    QComboBox *mComboBoxZSpecifierType;

    VertexPointer mCurrentVertex = nullptr;

    void setZSpecifierEditor( VertexPointer vertex );

};

#endif // REOSVERTEXZSPECIFIEREDITORWIDGET_H
