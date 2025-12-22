package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.desempenho.DesempenhoUsuarioResponse;
import com.treinamaisapi.common.dto.questao.request.QuestaoRequest;
import com.treinamaisapi.common.dto.questao.response.QuestaoResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Desempenho", description = "Desempenho do usuario ")
public interface DezempenhoControllerSwagger {


    @GetMapping("/{usuarioId}")
    ResponseEntity<DesempenhoUsuarioResponse> obterDesempenho(
            @PathVariable Long usuarioId
    );
}
