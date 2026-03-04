package com.treinamaisapi.controller.saude;

import com.treinamaisapi.controller.swagger.SaudeControllerSwagger;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/saude", produces = MediaType.APPLICATION_JSON_VALUE)
public class SaudeController implements SaudeControllerSwagger {

    @GetMapping
    @Override
    public String health() {
        return "ok";
    }

}
