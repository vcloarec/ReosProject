#ifndef REOSSTRUCTURE2DTOOLBAR_H
#define REOSSTRUCTURE2DTOOLBAR_H

#include <QToolBar>
#include "reosgui.h"

class QToolButton;
class ReosHydraulicStructure2DProperties;

class REOSGUI_EXPORT ReosStructure2dToolBar : public QToolBar
{
    Q_OBJECT
  public:
    explicit ReosStructure2dToolBar( QWidget *parent = nullptr );

    void setCurrentStructure2DPropertiesWidget( ReosHydraulicStructure2DProperties *structurePropertiesWidget );

  private:
    QToolButton *m3dButton = nullptr;
    QToolButton  *mEditButton = nullptr;
    QToolButton  *mRunButton = nullptr;
    QToolButton *mScalarButton = nullptr;
    QToolButton  *mVectorButton = nullptr;
    QToolButton  *mProfileButton = nullptr;
    QToolButton *mExportMesh = nullptr;
    ReosHydraulicStructure2DProperties *mPlaceHolder = nullptr;
};

#endif // REOSSTRUCTURE2DTOOLBAR_H
