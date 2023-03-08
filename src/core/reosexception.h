#ifndef REOSEXCEPTION_H
#define REOSEXCEPTION_H

#include <QString>

#define SIP_NO_FILE

class ReosException
{
  public:
    ReosException( const QString &message );
    virtual ~ReosException() = default;

    QString what() const;

  private:
    QString mWhat;
};

#endif // REOSEXCEPTION_H
