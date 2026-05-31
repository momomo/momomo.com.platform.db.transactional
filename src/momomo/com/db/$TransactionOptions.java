package momomo.com.db;

import java.sql.Connection;

/**
 * Yes, Java generics are not ideal to work with. 
 * The generics part is intended to allow for chaining.
 *
 * @param <Tx> must be the subclass that is extending {@link $Transaction <Tx>} and can not be any other subclass also extending this class.
 * @param <THIS> must be the subclass that is extending {@link $TransactionOptions <Tx,  THIS >} itself and can not be any other subclass also extending this class.
 *             
 * @author Joseph S.             
 */
public abstract class $TransactionOptions<THIS extends $TransactionOptions<THIS, Tx>, Tx extends $Transaction<Tx>> {
    
    /////////////////////////////////////////////////////////////////////
    protected THIS THIS() { return (THIS) this; }
    /////////////////////////////////////////////////////////////////////
    
    public abstract Tx create();
    
    /////////////////////////////////////////////////////////////////////
    // The options
    /////////////////////////////////////////////////////////////////////
    
    protected Propagation propagation = Propagation.REQUIRE;
    protected Isolation   isolation;
    protected Boolean     readOnly;
    protected Integer     timeout;
    
    public final THIS propagation(Propagation propagation) {
        this.propagation = propagation; return THIS();
    }
    
    public final THIS readOnly(boolean readOnly) {
        this.readOnly = readOnly; return THIS();
    }
    
    public final THIS isolation(Isolation isolation) {
        this.isolation = isolation; return THIS();
    }
    
    public final THIS timeout(Integer timeout) {
        this.timeout = timeout; return THIS();
    }
    
    /////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////
    
    /**
     * No fan of enums as they are very defined
     * 
     * @author Joseph S.
     */
    public static class Isolation {
        public static Isolation NONE             = new Isolation(Connection.TRANSACTION_NONE);
        public static Isolation READ_COMMITTED   = new Isolation(Connection.TRANSACTION_READ_COMMITTED);
        public static Isolation READ_UNCOMMITTED = new Isolation(Connection.TRANSACTION_READ_UNCOMMITTED);
        public static Isolation REPEATABLE_READ  = new Isolation(Connection.TRANSACTION_REPEATABLE_READ);
        public static Isolation SERIALIZABLE     = new Isolation(Connection.TRANSACTION_SERIALIZABLE);
    
        public final int delegate; 
        Isolation(int delegate) {
            this.delegate = delegate; 
        }
    }
    
    /////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////
    
    /**
     * No fan of enums as they are very defined. 
     * 
     * @author Joseph S.
     */
    public static class Propagation {
        public static Propagation NEW     = new Propagation();
        public static Propagation REQUIRE = new Propagation();
        public static Propagation SUPPORT = new Propagation();
    }
    
    /////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////
}


