package momomo.com.db;

import momomo.com.Lambda;
import momomo.com.exceptions.$DatabaseException;
import momomo.com.exceptions.$DatabaseTransactionalCommitException;
import momomo.com.exceptions.$DatabaseTransactionalRollbackException;

import java.util.ArrayList;

/**
 * Yes, Java generics are not ideal to work with. 
 * The generics part is intended to allow for chaining and exact type resolution. 
 * 
 * @param <Tx> must be the subclass that is extending {@link $Transaction < THIS >} itself and can not be any other subclass also extending this class.  
 *
 * @author Joseph S.           
 */
public abstract class $Transaction<Tx extends $Transaction<Tx>>  {
    private final $TransactionManager<Tx, ?> manager;
    
    /////////////////////////////////////////////////////////////////////
    private Tx THIS() { return (Tx) this; }
    /////////////////////////////////////////////////////////////////////
    
    // Can be null which means commit unless paramter commit to execute is passed
    private Boolean commit;
    
    // For supportTransaction Spring never sets it to complete why we have to do it ourselves to ensure we don't repeat on a terminated transaction
    private boolean rolled = false, committed = false;
    
    protected $Transaction(momomo.com.db.$TransactionManager<Tx, ?> manager) {
        this(manager, null);
    }
    
    protected $Transaction(momomo.com.db.$TransactionManager<Tx, ?> manager, Boolean commit) {
        this.manager = manager;
        this.commit  = commit;
    }
    
    /////////////////////////////////////////////////////////////////////
    
    public Tx rollback() {
        this.commit = false;            // One a rollback has been called, we disable autocommit regardless of the success of the outcome of the rollback call, just in case, and also to prevent the commit from occuring in execute 
        
        try {
            if ( !rolled ) {
                manager.rollback( THIS() );
                rolled = true;
            }
        }
        catch ( Throwable e ) {
            rolled = false;

            handleRollbackException(e);
        }

        after(rolled, afterRollback);

        return THIS();
    }
    /////////////////////////////////////////////////////////////////////

    public Tx commit() {
        try {
            if ( !committed ) {
                manager.commit( THIS() );

                committed = true;
            }
        } catch (Throwable e) {
            committed = false;

            handleCommitException(e);
        }

        after(committed, afterCommit);

        return THIS();
    }

    /////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////
    
    private final ArrayList<Lambda.VE<? extends Exception>> afterCommit = new ArrayList<>(1);
    public <E extends Exception> Tx afterCommit(Lambda.VE<E> lambda) {
        afterCommit.add(lambda); return THIS();
    }
    
    private final ArrayList<Lambda.VE<? extends Exception>> afterRollback = new ArrayList<>(1);
    public <E extends Exception> Tx afterRollback(Lambda.VE<E> lambda) {
        afterRollback.add(lambda); return THIS();
    }
    
    /////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////
    
    /* Override to do something else */
    protected void handleCommitException(Throwable e) throws $DatabaseException {
        throw new $DatabaseTransactionalCommitException(e);
    }
    
    /* Override to do something else */
    protected void handleRollbackException(Throwable e) throws $DatabaseException {
        throw new $DatabaseTransactionalRollbackException(e);
    }
    
    /* Override to do something else */
    protected void handleExecuteException(Throwable e) throws $DatabaseException {
        // SpringTransaction used to override this one to take care of TransactionTimeout using some exception magic, we don't but we leave the ability to hook in instead
    }
    
    /////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////
    
    /**
     * @param lambda no param lambda
     */
    public <E extends Throwable> void execute(Lambda.VE<E> lambda) throws E {
        execute(lambda.R1E());
    }
    
    /**
     * @param lambda 1 param lamda which passes 'this' Transaction
     */
    public <R, E extends Throwable> void execute(Lambda.V1E<Tx, E> lambda) throws E {
        execute(lambda.R1E());
    }
    
    /**
     * @param lambda no param lambda
     * @param commit true, false, or null where null leaves the default in, and false explicitly tells it not to commit automatically               
     */
    public <E extends Throwable> void execute(Lambda.VE<E> lambda, Boolean commit) throws E {
        execute(lambda.R1E(), commit);
    }
    
    /**
     * @param lambda 1 param lamda which passes 'this' Transaction
     * @param commit true, false, or null where null leaves the default in, and false explicitly tells it not to commit automatically               
     */
    public <R, E extends Throwable> void execute(Lambda.V1E<Tx, E> lambda, Boolean commit) throws E {
        execute(lambda.R1E(), commit);
    }
    
    /////////////////////////////////////////////////////////////////////
    
    /**
     * @param lambda 0 param lamda that returns, allowing you to return whatever the lambda wants to return such as created entity within the transaction
     */
    public <R, E extends Throwable> R execute(Lambda.RE<R, E> lambda) throws E {
        return execute(lambda.R1E());
    }
    /**
     * @param lambda 1 param lamda which passes 'this' Transaction that returns, allowing you to return whatever the lambda wants to return such as created entity within the transaction
     */
    public <R, E extends Throwable> R execute(Lambda.R1E<R, Tx, E> lambda) throws E {
        return execute(lambda, this.commit);
    }
    
    /**
     * @param lambda 1 param lamda which passes 'this' Transaction that returns, allowing you to return whatever the lambda wants to return such as created entity within the transaction
     * @param commit true, false, or null where null leaves the default in, and false explicitly tells it not to commit automatically
     */
    public <R, E extends Throwable> R execute(Lambda.RE<R, E> lambda, Boolean commit) throws E {
        return execute(lambda.R1E(), commit);
    }
    
    /**
     * @param lambda 1 param lamda which passes 'this' Transaction that returns, allowing you to return whatever the lambda wants to return such as created entity within the transaction
     * @param commit true, false, or null where null leaves the default in, and false explicitly tells it not to commit automatically
     */
    public <R, E extends Throwable> R execute(Lambda.R1E<R, Tx, E> lambda, Boolean commit) throws E {
        R result;
        try {
            result = lambda.call((Tx) this);
        }
        catch( Throwable a ) {
            // If user hasn't already committed in the lambda.call and possibly caused an error during commit Spring would have already rolled it back. 
            // If that's true, rolling back is safe anyway, and will cause a new error when rollback again which will throw a RollbackException. 
            
            try {
                rollback();

                throw a;
            }
            catch (Throwable b) {
                handleExecuteException(b);      // User might opt to throw something else here, like a $DatabaseException(...). We use for force wrap exceptions to $DatbaseExceptions, now we opt to keep E pure.          
                
                throw b;
            }
        }

        // Unless rolled already. If passed in true, always commit. If this.commit is not false, and commit is not false
        if ( !rolled && ( Boolean.TRUE.equals(commit) || (!Boolean.FALSE.equals(this.commit) && !Boolean.FALSE.equals(commit)) ) ) {
            commit();                     
        }

        return result;
    }

    /////////////////////////////////////////////////////////////////////

    public Tx autocommit(boolean commit) {
        this.commit = commit; return THIS();
    }

    private void after(boolean success, ArrayList<Lambda.VE<? extends Exception>> lambdas) {
        if (success) {
            for (Lambda.VE<? extends Exception> lambda : lambdas) {
                try {
                    lambda.call();
                } catch (Exception e) {
                    throw new $DatabaseException(e);
                }
            }
        }
    }
    
}
