#ifndef REOSVERTEXZSPECIFIEREDITORWIDGET_H
#define REOSVERTEXZSPECIFIEREDITORWIDGET_H

#include <QDialog>
#include <QPushButton>
#include <QMessageBox>

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
    void stop();
    void vertexHasToBeRemoved( VertexPointer vert );

  signals:
    void specifierHasChanged();

  private slots:
    void currentEntryChanged( int index );
    void apply();

  private:
    Ui::ReosVertexZSpecifierEditorWidget *ui;

    QList<ReosVertexZSpecifierEntryWidget *> mEntryWidgets;
    ReosVertexZSpecifierEntryWidgetModel *mEntriesModel;
    ReosVertexZSpecifierEntryWidget *mCurrentEntryWidget = nullptr;

    ReosFormText *mVertexReference;
    QComboBox *mComboBoxZSpecifierType;

    VertexPointer mCurrentVertex = nullptr;

    void setZSpecifierEditor( VertexPointer vertex );
    void updateCurrentVertex();

    //! check if the current z specifier and reference vertices are compatible with the current vertex
    //! If not, dispaly a warning message box and disable the apply button
    bool checkUncompatibility();

    //! check if the current z specifier and reference vertices are compatible with the current vertex
    //! If yes, enable the apply button
    bool checkCompatibility();

};

#endif // REOSVERTEXZSPECIFIEREDITORWIDGET_H
