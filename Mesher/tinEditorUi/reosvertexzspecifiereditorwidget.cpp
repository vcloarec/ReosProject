#include "reosvertexzspecifiereditorwidget.h"
#include "ui_reosvertexzspecifiereditorwidget.h"

ReosVertexZSpecifierEditorWidget::ReosVertexZSpecifierEditorWidget( ReosMap *map, ReosMapMeshEditorItemDomain *domain, QWidget *parent ) :
  QDialog( parent ),
  ui( new Ui::ReosVertexZSpecifierEditorWidget )
{
  ui->setupUi( this );

  mEntryWidgets.append( new ReosVertexZSpecifierSimpleValueWidget( this ) );
  ReosVertexZSpecifierDependentOtherVertexWidget *e = new ReosVertexZSpecifierSlopeWidget( map, domain, this );
  e->disableTakeLastVertex();
  mEntryWidgets.append( e );
  e = new ReosVertexZSpecifierGapWidget( map, domain, this );
  e->disableTakeLastVertex();
  mEntryWidgets.append( e );
  mEntryWidgets.append( new ReosVertexZSpecifierInterpolationWidget( map, domain, this ) );


  mEntriesModel = new ReosVertexZSpecifierEntryWidgetModel( mEntryWidgets, this );
  mEntriesModel->setTextDisplayed( true );
  mComboBoxZSpecifierType = new QComboBox;
  mComboBoxZSpecifierType->setModel( mEntriesModel );

  ui->groupBoxZValue->layout()->addWidget( mComboBoxZSpecifierType );

  for ( auto w : mEntryWidgets )
  {
    ui->groupBoxZValue->layout()->addWidget( w );
    w->hide();
  }


  connect( mComboBoxZSpecifierType, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosVertexZSpecifierEditorWidget::currentEntryChanged );

}

ReosVertexZSpecifierEditorWidget::~ReosVertexZSpecifierEditorWidget()
{
  delete ui;
}

void ReosVertexZSpecifierEditorWidget::setVertex( VertexPointer vertex )
{
  if ( mCurrentVertex )
    static_cast<ReosMeshItemVertex *>( mCurrentVertex->graphicPointer() )->setCurrent( false );

  mCurrentVertex = vertex;

  if ( !vertex )
    return;

  static_cast<ReosMeshItemVertex *>( vertex->graphicPointer() )->setCurrent( true );
  ui->vertexReference->setText( vertexReferenceText( mCurrentVertex ) );
  setZSpecifierEditor( mCurrentVertex );
}

void ReosVertexZSpecifierEditorWidget::currentEntryChanged( int index )
{
  for ( int i = 0; i < mEntryWidgets.count(); ++i )
  {
    if ( i == index )
      mEntryWidgets.at( i )->start();
    else
      mEntryWidgets.at( i )->stop();
  }
}

void ReosVertexZSpecifierEditorWidget::setZSpecifierEditor( VertexPointer vertex )
{
  ReosVertexZSpecifier *spec = vertex->zSpecifier();

  int index = -1;
  for ( auto entry : mEntryWidgets )
  {
    if ( entry->type() == spec->type() )
    {
      entry->clear();
      index = mEntryWidgets.indexOf( entry );
      entry->setSpecifier( vertex->zSpecifier() );
    }
    else
    {
      entry->clear();
    }
  }

  mComboBoxZSpecifierType->setCurrentIndex( index );

}
