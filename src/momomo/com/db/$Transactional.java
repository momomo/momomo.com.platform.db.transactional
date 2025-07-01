package momomo.com.db;

import momomo.com.Lambda;

/**
 * Yes, Java generics are not ideal to work with. 
 * The generics part is intended to allow for chaining.   
 * 
 * @param <Tx> must be the subclass that is extending {@link $Transaction <Tx>} and can not be any other subclass also extending this class.
 * @param <TxO> must be the subclass that is extending {@link $TransactionOptions <Tx, TxO>} and can not be any other subclass also extending this class.            
 * 
 * @author Joseph S.
 */
public interface $Transactional<Tx extends $Transaction<Tx>, TxO extends $TransactionOptions<TxO, Tx>> {
    
    /////////////////////////////////////////////////////////////////////
    
    TxO options();
    
    /////////////////////////////////////////////////////////////////////
    // options
    /////////////////////////////////////////////////////////////////////
    
    default TxO requireOptions() {
        return options().propagation($TransactionOptions.Propagation.REQUIRE);
    }
    
    default TxO newOptions() {
        return options().propagation($TransactionOptions.Propagation.NEW);
    }
    
    
    default TxO supportOptions() {
        return options().propagation($TransactionOptions.Propagation.SUPPORT);
    }
    
    /////////////////////////////////////////////////////////////////////
    // requireTransaction
    /////////////////////////////////////////////////////////////////////
    
    default Tx requireTransaction() {
        return requireOptions().create();
    }
    
    default <E extends Throwable> void requireTransaction(Lambda.VE<E> lambda) throws E {
        requireTransaction().execute(lambda);
    }
    default <E extends Throwable> void requireTransaction(Lambda.VE<E> lambda, Boolean commit) throws E {
        requireTransaction().execute(lambda, commit);
    }
    
    default <R, E extends Throwable> void requireTransaction(Lambda.V1E<? super Tx, E> lambda) throws E {
        requireTransaction().execute(lambda);
    }
    default <R, E extends Throwable> void requireTransaction(Lambda.V1E<? super Tx, E> lambda, Boolean commit) throws E {
        requireTransaction().execute(lambda, commit);
    }
    
    default <R, E extends Throwable> R requireTransaction(Lambda.RE<R, E> lambda) throws E {
        return requireTransaction().execute(lambda);
    }
    default <R, E extends Throwable> R requireTransaction(Lambda.RE<R, E> lambda, Boolean commit) throws E {
        return requireTransaction().execute(lambda, commit);
    }
    
    default <R, E extends Throwable> R requireTransaction(Lambda.R1E<R, ? super Tx, E> lambda) throws E {
        return requireTransaction().execute(lambda);
    }
    default <R, E extends Throwable> R requireTransaction(Lambda.R1E<R, ? super Tx, E> lambda, Boolean commit) throws E {
        return requireTransaction().execute(lambda, commit);
    }
    
    /////////////////////////////////////////////////////////////////////
    // newTransaction
    /////////////////////////////////////////////////////////////////////
    
    default Tx newTransaction() {
        return newOptions().create();
    }
    
    default <E extends Throwable> void newTransaction(Lambda.VE<E> lambda) throws E {
        newTransaction().execute(lambda);
    }
    default <E extends Throwable> void newTransaction(Lambda.VE<E> lambda, Boolean commit) throws E {
        newTransaction().execute(lambda, commit);
    }
    
    default <R, E extends Throwable> void newTransaction(Lambda.V1E<? super Tx, E> lambda) throws E {
        newTransaction().execute(lambda);
    }
    default <R, E extends Throwable> void newTransaction(Lambda.V1E<? super Tx, E> lambda, Boolean commit) throws E {
        newTransaction().execute(lambda, commit);
    }
    
    default <R, E extends Throwable> R newTransaction(Lambda.RE<R, E> lambda) throws E {
        return newTransaction().execute(lambda);
    }
    default <R, E extends Throwable> R newTransaction(Lambda.RE<R, E> lambda, Boolean commit) throws E {
        return newTransaction().execute(lambda, commit);
    }
    
    default <R, E extends Throwable> R newTransaction(Lambda.R1E<R, ? super Tx, E> lambda) throws E {
        return newTransaction().execute(lambda);
    }
    default <R, E extends Throwable> R newTransaction(Lambda.R1E<R, ? super Tx, E> lambda, Boolean commit) throws E {
        return newTransaction().execute(lambda, commit);
    }
    
    /////////////////////////////////////////////////////////////////////
    // supportTransaction
    /////////////////////////////////////////////////////////////////////    
    
    default Tx supportTransaction() {
        return supportOptions().create();
    }
    
    default <E extends Throwable> void supportTransaction(Lambda.VE<E> lambda) throws E {
        supportTransaction().execute(lambda);
    }
    default <E extends Throwable> void supportTransaction(Lambda.VE<E> lambda, Boolean commit) throws E {
        supportTransaction().execute(lambda, commit);
    }
    
    default <R, E extends Throwable> void supportTransaction(Lambda.V1E<? super Tx, E> lambda) throws E {
        supportTransaction().execute(lambda);
    }
    default <R, E extends Throwable> void supportTransaction(Lambda.V1E<? super Tx, E> lambda, Boolean commit) throws E {
        supportTransaction().execute(lambda, commit);
    }
    
    default <R, E extends Throwable> R supportTransaction(Lambda.RE<R, E> lambda) throws E {
        return supportTransaction().execute(lambda);
    }
    default <R, E extends Throwable> R supportTransaction(Lambda.RE<R, E> lambda, Boolean commit) throws E {
        return supportTransaction().execute(lambda, commit);
    }
    
    default <R, E extends Throwable> R supportTransaction(Lambda.R1E<R, ? super Tx, E> lambda) throws E {
        return supportTransaction().execute(lambda);
    }
    default <R, E extends Throwable> R supportTransaction(Lambda.R1E<R, ? super Tx, E> lambda, Boolean commit) throws E {
        return supportTransaction().execute(lambda, commit);
    }
    
    /////////////////////////////////////////////////////////////////////
    
}
