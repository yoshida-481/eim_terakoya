package jp.co.ctc_g.eim.app.document.presentation.controller;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import jp.co.ctc_g.eim.app.document.business.service.DocumentFormService;
import jp.co.ctc_g.eim.app.form.business.domain.FormDomain;
import jp.co.ctc_g.eim.app.form.presentation.form.FormForm;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;

/**
 * ドキュメントフォームコントロールクラス
 * Spring MVC のコントローラクラスです。
 * @since Ver6.8
 */
@Controller
@RequestMapping(value = "/rest/documentform")
public class DocumentFormController extends DocumentRestController {
	
	/** ドキュメントフォームに関する操作を行うサービス */
	private DocumentFormService documentFormService;
	
	/**
	 * ドキュメントフォームオブジェクトを作成します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.DocumentFormService#create(FormDomain form) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.DocumentFormService#create(FormDomain form)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/create", method=RequestMethod.POST)
	@ResponseBody
	public FormDomain create(@RequestBody FormForm dto) throws Exception {
		FormDomain newForm = null;
		try{
			
			newForm = documentFormService.create(dto.convert(new FormDomain()));
			
		} finally {
			clearFlexContext();
		}
		return newForm;
	}

	/**
	 * ドキュメントフォームオブジェクトを更新します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.DocumentFormService#update(FormDomain form) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.DocumentFormService#update(FormDomain form)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/update", method=RequestMethod.POST)
	@ResponseBody
	public FormDomain update(@RequestBody FormForm dto) throws Exception {
		FormDomain newForm = null;
		try{
			
			newForm = documentFormService.update(dto.convert(new FormDomain()));
			newForm.setModificationDate(
					convertDateClass(newForm.getModificationDate()));
			
		} finally {
			clearFlexContext();
		}
		
		return newForm;
	}

	/**
	 * ドキュメントフォームオブジェクトを取得します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.DocumentFormService#getById(long id) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.DocumentFormService#getById(long id)
	 * @since Ver6.8
	 */
	@RequestMapping(value = "/getById", method=RequestMethod.GET)
	@ResponseBody
	public FormDomain getById(@RequestParam("id") long id) throws Exception {
		FormDomain form = null;
		try{
			form = documentFormService.getById(id);
			form.setModificationDate(
					convertDateClass(form.getModificationDate()));
		} finally {
			clearFlexContext();
		}
		
		return form;
	}

	/**
	 * 指定されたIDの帳票に対応するすべてのリビジョンのオブジェクトIDを取得します。<br>
	 * 本メソッドは{@link  jp.co.ctc_g.eim.app.document.business.service.DocumentFormService#getAllRevisionById(long id) Service}へ処理を委譲します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.DocumentFormService#getAllRevisionById(long id)
	 * @since Ver6.11
	 */
	@RequestMapping(value = "/getAllRevisionById", method=RequestMethod.GET)
	@ResponseBody
	public List<Long> getAllRevisionById(@RequestParam("id") long id) throws Exception {
		List<Long> objectIdList = null;
		try{
			List<ObjectDomain> objectList = documentFormService.getAllRevisionById(id);
			objectIdList = new ArrayList<>(objectList.size());
			for (ObjectDomain object : objectList) {
				objectIdList.add(object.getId());
			}
		} finally {
			clearFlexContext();
		}
		
		return objectIdList;
	}

	/**
	 * ドキュメントフォームに関する操作を行うサービスを取得します。
	 * @return ドキュメントフォームに関する操作を行うサービス
	 */
	public DocumentFormService getDocumentFormService() {
		return documentFormService;
	}

	/**
	 * ドキュメントフォームに関する操作を行うサービスを設定します。
	 * @param documentFormService ドキュメントフォームに関する操作を行うサービス
	 */
	public void setDocumentFormService(DocumentFormService documentFormService) {
		this.documentFormService = documentFormService;
	}
	
	/**
	 * 日付型を変更（java.sql.Date → java.util.Date）する
	 */
    private Date convertDateClass(Date date) throws Exception {
        // sql.dateの場合、変換処理を行う
        if (date != null && date.getClass().getName().equals("java.sql.Date")) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            date = sdf.parse(sdf.format(date));
        }
        return date;
    } 
	
}
