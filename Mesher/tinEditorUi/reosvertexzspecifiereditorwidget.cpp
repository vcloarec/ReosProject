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
  connect( ui->buttonBox->button( QDialogButtonBox::Apply ), &QPushButton::pressed, this, &ReosVertexZSpecifierEditorWidget::apply );
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

  updateCurrentVertex();

}

void ReosVertexZSpecifierEditorWidget::stop()
{
  if ( mCurrentVertex )
    static_cast<ReosMeshItemVertex *>( mCurrentVertex->graphicPointer() )->setCurrent( false );

  if ( mCurrentEntryWidget )
    mCurrentEntryWidget->stop();
  hide();
}

void ReosVertexZSpecifierEditorWidget::vertexHasToBeRemoved(VertexPointer vert)
{
    for ( auto e : mEntryWidgets )
        e->vertexHasToBeRemoved( vert );
    if ( mCurrentVertex == vert )
        mCurrentVertex = nullptr;
}

void ReosVertexZSpecifierEditorWidget::currentEntryChanged( int index )
{
    mCurrentEntryWidget = nullptr;
    for ( int i = 0; i < mEntryWidgets.count(); ++i )
    {
        if ( i == index )
        {
            mCurrentEntryWidget = mEntryWidgets.at( i );
      mCurrentEntryWidget->start();
    }
    else
      mEntryWidgets.at( i )->stop();
  }
  checkCompatibility();
}

void ReosVertexZSpecifierEditorWidget::apply()
{
    if ( !checkUncompatibility() )
        return;
    mCurrentEntryWidget->assignZSpecifier( mCurrentVertex );
    updateCurrentVertex();
    emit specifierHasChanged();
}

void ReosVertexZSpecifierEditorWidget::setZSpecifierEditor( VertexPointer vertex )
{
    ReosVertexZSpecifier *spec = vertex->zSpecifier();
    mCurrentEntryWidget = nullptr;
    int index = -1;
    for ( auto entry : mEntryWidgets )
    {
        if ( entry->type() == spec->type() )
    {
      entry->clear();
      index = mEntryWidgets.indexOf( entry );
      mCurrentEntryWidget = entry;
    }
    else
    {
      entry->clear();
    }
    entry->setSpecifier( vertex->zSpecifier() );
  }

  if ( index == mComboBoxZSpecifierType->currentIndex() )
    currentEntryChanged( index );
  else
    mComboBoxZSpecifierType->setCurrentIndex( index );
}

void ReosVertexZSpecifierEditorWidget::updateCurrentVertex()
{
  if ( !mCurrentVertex )
    return;

  static_cast<ReosMeshItemVertex *>( mCurrentVertex->graphicPointer() )->setCurrent( true );
  ui->vertexReference->setText( vertexReferenceText( mCurrentVertex ) );
  setZSpecifierEditor( mCurrentVertex );
  checkCompatibility();
}

bool ReosVertexZSpecifierEditorWidget::checkUncompatibility()
{
  if ( !( mCurrentVertex && mCurrentEntryWidget->factory().IsCompatibleZSpecifier( mCurrentVertex ) ) )
  {
    ui->buttonBox->button( QDialogButtonBox::Apply )->setEnabled( false );
    QMessageBox::warning( this, tr( "Z value specifier" ), tr( "Uncompatible type and reference with the associated vertex" ) );
    return false;
  }

  return true;
}

bool ReosVertexZSpecifierEditorWidget::checkCompatibility()
{
  if ( mCurrentVertex && mCurrentEntryWidget->factory().IsCompatibleZSpecifier( mCurrentVertex ) )
  {
    ui->buttonBox->button( QDialogButtonBox::Apply )->setEnabled( true );
    return true;
  }

  return false;
}
