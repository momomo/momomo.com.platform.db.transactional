package momomo.com.db;

/**
 * Yes, Java generics are not ideal to work with. 
 * The generics part is intended to allow for chaining and exact type resolution. 
 * 
 * @param <Tx> must be the subclass that is extending {@link $Transaction <Tx>} and can not be any other subclass also extending this class.
 * @param <TxO> must be the subclass that is extending {@link $TransactionOptions <Tx, TxO>} and can not be any other subclass also extending this class.
 *            
 * @author Joseph S.
 */
public interface $TransactionManager<Tx extends $Transaction<Tx>, TxO extends $TransactionOptions<TxO, Tx>> {
    void commit  (Tx transaction);
    void rollback(Tx transaction);
}
