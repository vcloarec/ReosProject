#include "reosstructure2dtoolbar.h"
#include "QToolButton"

#include "reoshydraulicstructure2dproperties.h"

ReosStructure2dToolBar::ReosStructure2dToolBar( QWidget *parent )
  : QToolBar( parent )
  , m3dButton( new QToolButton( this ) )
  , mEditButton( new QToolButton( this ) )
  , mRunButton( new QToolButton( this ) )
  , mScalarButton( new QToolButton( this ) )
  , mVectorButton( new QToolButton( this ) )
  , mProfileButton( new QToolButton( this ) )
  , mExportMesh( new QToolButton( this ) )
{
  setObjectName( "structure-2D-tool-bar" );
  setWindowTitle( tr( "Structure 2D" ) );

  addWidget( mEditButton );
  addSeparator();
  addWidget( mRunButton );
  addSeparator();
  addWidget( mScalarButton );
  addWidget( mVectorButton );
  addSeparator();
  addWidget( m3dButton );
  addWidget( mProfileButton );
  addSeparator();
  addWidget( mExportMesh );

  mPlaceHolder = new ReosHydraulicStructure2DProperties( nullptr, ReosGuiContext( this ) );
  mPlaceHolder->hide();

  setCurrentStructure2DPropertiesWidget( mPlaceHolder );
}

static void setAction( QToolButton *button, QAction *action )
{
  const QList<QAction *> oldActions = button->actions();
  for ( QAction *oldAction : oldActions )
    button->removeAction( oldAction );

  button->setDefaultAction( action );
}

void ReosStructure2dToolBar::setCurrentStructure2DPropertiesWidget( ReosHydraulicStructure2DProperties *structurePropertiesWidget )
{
  if ( !structurePropertiesWidget )
    structurePropertiesWidget = mPlaceHolder;

  const QList<QAction *> oldActions = actions();
  for ( QAction *oldAction : oldActions )
    removeAction( oldAction );

  addAction( structurePropertiesWidget->actionEditStructure() );
  addSeparator();
  addAction( structurePropertiesWidget->actionRunSimulation() );
  addSeparator();
  addAction( structurePropertiesWidget->scalarWidgetAction() );
  addAction( structurePropertiesWidget->vectorWidgetAction() );
  addSeparator();
  addAction( structurePropertiesWidget->action3DView() );
  addAction( structurePropertiesWidget->actionProfiles() );
  addSeparator();
  addAction( structurePropertiesWidget->actionExportAsMesh() );
}
