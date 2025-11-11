package jp.co.ctc_g.eim.app.document.common.aop.plugin.impl;

import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain;
import jp.co.ctc_g.eim.framework2.common.aop.plugin.ParameterConverterPlugIn;

/**
 * 【ドキュメントAPI】
 * 入力値としてSecurityDomainを受け取り、ObjectDomainインスタンスを生成するプラグインクラスです。
 * @since Ver1.0
 */
public class SecurityDomainToObjectConverterPlugInImpl implements ParameterConverterPlugIn {
	/**
	 * SecurityDomainを受け取り、ObjectDomainインスタンスをnewして返却します。
	 * @param source SecurityDomainです。
	 * @return IDをコンストラクタに設定してnewしたObjectDomainインスタンスです。
	 * @since Ver1.0
	 */
	public Object convert(Object source) throws Exception {
		SecurityDomain securityDomain = (SecurityDomain)source;
		ObjectDomain objectDomain = new ObjectDomain(securityDomain.getId());
		objectDomain.setName(securityDomain.getDefinitionName());
		return objectDomain;
	}
}
