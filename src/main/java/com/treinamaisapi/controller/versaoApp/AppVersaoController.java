package com.treinamaisapi.controller.versaoApp;

import com.treinamaisapi.common.dto.versaoApp.AppVersionDTO;
import com.treinamaisapi.controller.swagger.AppVersaoControllerSwagger;
import com.treinamaisapi.service.AppVersion.AppVersionService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/app", produces = MediaType.APPLICATION_JSON_VALUE)
public class AppVersaoController implements AppVersaoControllerSwagger {

    private final AppVersionService service;

    @Override
    @GetMapping("/version")
    public AppVersionDTO getVersion() {
        return service.getLatestVersion();
    }

}
