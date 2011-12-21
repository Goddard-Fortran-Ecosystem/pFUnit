#define THROW(exception) call throw(exception)
#define TRY call try()
#define CATCH(exception) if (catch(exception)) then
#define OR_CATCH(exception) elseif (catch(exception)) then
#define END_TRY endif; call endTry()


