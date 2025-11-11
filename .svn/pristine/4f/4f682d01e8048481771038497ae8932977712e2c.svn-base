/**
 * check_scr_Model
 */
var check_scr_Model = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeModel;
	var f = new F();
	
	// ビュー
	const viewName = "check_scr_View";
	const viewId = "#" + viewName;
	var view = null;
	
	/*****************************************************************
	 * 初期処理
	 ****************************************************************/
	jQuery(document).on('pageinit', viewId, function(ev) {
		
		/**
		 * 変数とセレクタの関連付け
		 */
		// ビュー
		view = $(viewId);
		
	});
	
	/*****************************************************************
	 * 関数定義
	 ****************************************************************/

	/**
	 * 履歴情報の取得
	 * post_WorkFlowHistory (punapproved_scr_list_itemId)
	 */
	function post_WorkFlowHistory (punapproved_scr_list_itemId) {
		// 共通モデルメソッドを呼び出し
		f.post_WorkFlowHistory (punapproved_scr_list_itemId, view);
	}
	
	/**
	 * 差戻し情報の取得
	 * post_doEvent_back ( papproved_scr_list_itemId, 
	 *                     comment, pfolderList_scr_timing, pfolderList_scr_statusId, pfolderList_scr_finalApprove, 
	 *                     pfolderList_scr_statusMDateLong, pfolderList_scr_forcastStatusTypeId)
	 */
	function post_doEvent_back ( papproved_scr_list_itemId, 
								 comment,
								 pfolderList_scr_timing,
								 pfolderList_scr_statusId,
								 pfolderList_scr_finalApprove,
								 pfolderList_scr_statusMDateLong,
								 pfolderList_scr_forcastStatusTypeId) {
		
		// しばらくお待ち下さい
		f.show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00017'];

		var post_data = {
				timing : pfolderList_scr_timing, 
				statusId : pfolderList_scr_statusId,  
				finalApprove : pfolderList_scr_finalApprove, 
				statusMDateLong : pfolderList_scr_statusMDateLong,
				forcastStatusTypeId : pfolderList_scr_forcastStatusTypeId,
				immediteMailTypeId : '-12003',
				objId : papproved_scr_list_itemId,
				approverId : '',
				functionType : 'back',
				baseEventTypeId : '-14005',
				comment : comment,
		}; 
		
		$.ajax({
			url : url, 
			type : 'POST',
			data : post_data, 
			dataType : 'xml',
			cache : false,
			contentType : 'application/x-www-form-urlencoded' ,
			success : function(xml){
				post_doEvent_SuccessHandler(xml);
			},
			statusCode : { 
				// on('timeout')
				401: function(){ f.raiseSessionTimeoutError();},
				// on('not_found')
				404: function(){ f.raiseNotFoundError();},
				// on('internal_error')
				500: function(){ f.raiseInternalServerError();},
			},
			error : function(xhr, status, error){
				post_doEvent_ErrorHandler();
			},
		});
	}
	
	
	/**
	 * 承認者一覧の取得
	 * post_dspApprover (pfolderList_scr_ObjId, pfolderList_scr_forcastStatusTypeId, papprovers)
	 */
	function post_dspApprover (pfolderList_scr_ObjId, pfolderList_scr_forcastStatusTypeId, papprovers) {
		
		// しばらくお待ち下さい
		f.show_wait();
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00018'];

		var post_data = {
				statusTypeId : pfolderList_scr_forcastStatusTypeId,
				objId : pfolderList_scr_ObjId,
				selectedCheckBox: papprovers,
		}; 
		
		$.ajax({
			url : url, 
			type : 'POST',
			data : post_data, 
			dataType : 'xml',
			cache : false,
			contentType : 'application/x-www-form-urlencoded' ,
			success : function(xml){
				post_dspApproverSuccessHandler(xml);
			},
			statusCode : { 
				// on('timeout')
				401: function(){ f.raiseSessionTimeoutError();},
				// on('not_found')
				404: function(){ f.raiseNotFoundError();},
				// on('internal_error')
				500: function(){ f.raiseInternalServerError();},
			},
			error : function(xhr, status, error){
				post_dspApproverErrorHandler();
			},
		});
	}

	/**
	 * 承認の実行
	 * post_doEvent_approve ( papproved_scr_list_itemId, 
	 *                        comment, pfolderList_scr_timing, pfolderList_scr_statusId, pfolderList_scr_finalApprove, 
	 *                        pfolderList_scr_statusMDateLong, pfolderList_scr_forcastStatusTypeId, papprovers_list)
	 */
	function post_doEvent_approve ( papproved_scr_list_itemId, 
								 	comment,
								 	pfolderList_scr_timing,
								 	pfolderList_scr_statusId,
								 	pfolderList_scr_finalApprove,
								 	pfolderList_scr_statusMDateLong,
								 	pfolderList_scr_forcastStatusTypeId,
								 	papprovers_list) {
		
		var fapproverId = "";
		var fimmediateMailTypeId = "";
		var fimmediateAppMailTypeId = "";
		var faccumulateMailTypeId = "";
		var fnothingMailTypeId = "";
		var post_data;
		
		if (pfolderList_scr_finalApprove == true)
		{
			fimmediateMailTypeId = '-12005';
			fimmediateAppMailTypeId ='-12002';
			faccumulateMailTypeId = '-12005';
			fnothingMailTypeId = '-12005';
		}
		else 
		{
			fimmediateMailTypeId = '-12001';
			faccumulateMailTypeId = '-12001';
			fnothingMailTypeId = '-12001';
		}
		
		// しばらくお待ち下さい
		f.show_wait();
		
		jQuery.each(papprovers_list, function(i, object) {
			if(fapproverId == ""){
				fapproverId += "1:" + object.approverId;
			}else{
				fapproverId += ",1:" + object.approverId;
			}
		});
		
		var url = CONST_URL_PATH + cmnJSPAPI['JSPAPI_00017'];

		if (pfolderList_scr_timing == "0")
		{
			post_data = {
				timing : pfolderList_scr_timing, 
				statusId : pfolderList_scr_statusId,  
				finalApprove : pfolderList_scr_finalApprove, 
				statusMDateLong : pfolderList_scr_statusMDateLong,
				forcastStatusTypeId : pfolderList_scr_forcastStatusTypeId,
				objId : papproved_scr_list_itemId,
				approverId : fapproverId,
				functionType : 'approve',
				immediateMailTypeId : fimmediateMailTypeId,
				immediateAppMailTypeId : fimmediateAppMailTypeId,
				baseEventTypeId : '-14002',
				comment : comment,
			}; 
		}
		else if(pfolderList_scr_timing == "1")
		{
			post_data = {
				timing : pfolderList_scr_timing, 
				statusId : pfolderList_scr_statusId,  
				finalApprove : pfolderList_scr_finalApprove, 
				statusMDateLong : pfolderList_scr_statusMDateLong,
				forcastStatusTypeId : pfolderList_scr_forcastStatusTypeId,
				objId : papproved_scr_list_itemId,
				approverId : fapproverId,
				functionType : 'approve',
				accumulateMailTypeId : faccumulateMailTypeId,
				baseEventTypeId : '-14002',
				comment : comment,
			}; 
		}
		else
		{
			post_data = {
					timing : pfolderList_scr_timing, 
					statusId : pfolderList_scr_statusId,  
					finalApprove : pfolderList_scr_finalApprove, 
					statusMDateLong : pfolderList_scr_statusMDateLong,
					forcastStatusTypeId : pfolderList_scr_forcastStatusTypeId,
					objId : papproved_scr_list_itemId,
					approverId : fapproverId,
					functionType : 'approve',
					nothingMailTypeId : fnothingMailTypeId,
					baseEventTypeId : '-14002',
					comment : comment,
				}; 
		}
		
		$.ajax({
			url : url, 
			type : 'POST',
			data : post_data, 
			dataType : 'xml',
			cache : false,
			contentType : 'application/x-www-form-urlencoded' ,
			success : function(xml){
				post_doEvent_SuccessHandler(xml);
			},
			statusCode : { 
				// on('timeout')
				401: function(){ f.raiseSessionTimeoutError();},
				// on('not_found')
				404: function(){ f.raiseNotFoundError();},
				// on('internal_error')
				500: function(){ f.raiseInternalServerError();},
			},
			error : function(xhr, status, error){
				post_doEvent_ErrorHandler();
			},
		});
	}
	
	/**
	 * 未承認一覧情報を取得
	 * post_dspApproveDocumentList ()
	 */
	function post_dspApproveDocumentList () {
		// 共通モデルメソッドを呼び出し
		f.post_dspApproveDocumentList (view);
	}
		
	/*****************************************************************
	 * サーバ呼び出し結果
	 ****************************************************************/
	/**
	 * 差戻し情報の取得
	 * 成功
	 */
	function post_doEvent_SuccessHandler(xml) {
		f.postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('post_doEventError', xml);
			f_error_found = true;
		});
		if (!f_error_found){
			view.trigger('post_doEventSuccess', xml);
		};
	}
	
	/**
	 * 差戻し情報の取得
	 * エラー
	 */
	function post_doEvent_ErrorHandler() {
		f.postProcess();
		f.showServerDown();
	}
	
	/**
	 * 承認者一覧の取得
	 * 成功
	 */
	function post_dspApproverSuccessHandler(xml) {
		f.postProcess();
		
		// エラーメッセージを確認
		var f_error_found = false;
		$(xml).find("error").each(function(){
			view.trigger('post_dspApproverError', xml);
			f_error_found = true;
		});
		if (!f_error_found){
			view.trigger('post_dspApproverSuccess', xml);
		};
	}
	
	/**
	 * 承認者一覧の取得
	 * エラー
	 */
	function post_dspApproverErrorHandler() {
		f.postProcess();
		f.showServerDown();
	}
	
	return {
		post_WorkFlowHistory : post_WorkFlowHistory,
		post_doEvent_back : post_doEvent_back,
		post_doEvent_approve : post_doEvent_approve,
		post_dspApprover : post_dspApprover,
		post_dspApproveDocumentList : post_dspApproveDocumentList,
	};
}();

